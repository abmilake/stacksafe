;; stacksafe.clar
;; A simple vault contract "stacksafe" supporting:
;;  - STX deposits / withdrawals (two-step safe pattern)
;;  - SIP-010 FT deposits / withdrawals (two-step safe pattern)
;;  - Admin (deployer) with pause/unpause and emergency withdrawal

;; Define SIP-010 Fungible Token trait
(define-trait ft-trait
  (
    ;; Transfer from the caller to a new principal
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    ;; Get the token balance of the specified principal
    (get-balance (principal) (response uint uint))
    ;; Get the current balance of the contract
    (get-total-supply () (response uint uint))
    ;; Get the token's name
    (get-name () (response (string-ascii 32) uint))
    ;; Get the token's symbol
    (get-symbol () (response (string-ascii 32) uint))
    ;; Get the token's decimals
    (get-decimals () (response uint uint))
    ;; Get the token URI
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-POSITIVE (err u101))
(define-constant ERR-INSUFFICIENT (err u102))
(define-constant ERR-PAUSED (err u103))
(define-constant ERR-NO-DEPOSIT (err u104))

;; -------------------------
;; Admin & pause
;; -------------------------
(define-data-var admin principal tx-sender) ;; deployer becomes initial admin
(define-data-var paused bool false)

(define-read-only (get-admin)
  (ok (var-get admin)))

(define-read-only (is-paused)
  (ok (var-get paused)))

(define-private (assert-admin)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
    (ok true)))

(define-public (set-admin (new-admin principal))
  (begin
    (try! (assert-admin))
    (asserts! (not (is-eq new-admin tx-sender)) ERR-UNAUTHORIZED)
    (var-set admin new-admin)
    (ok true)))

(define-public (pause)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
    (try! (assert-admin))
    (var-set paused true)
    (ok true)))

(define-public (unpause)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
    (try! (assert-admin))
    (var-set paused false)
    (ok true)))

;; -------------------------
;; Storage: user balances
;; -------------------------
;; STX balances: map owner -> uint
(define-map stx-balances
  {owner: principal}
  {balance: uint})

;; FT balances: map (owner, token-contract) -> uint
(define-map ft-balances
  {owner: principal, token: principal}
  {balance: uint})

;; Track how much STX the contract has "accounted" (bookkeeping)
(define-map accounted-stx
  {dummy: bool}
  {amount: uint})

;; -------------------------
;; Helper functions
;; -------------------------

(define-read-only (get-stx-balance-of (owner principal))
  (ok (default-to u0 (get balance (map-get? stx-balances {owner: owner})))))

(define-read-only (get-ft-balance-of (owner principal) (token principal))
  (ok (default-to u0 (get balance (map-get? ft-balances {owner: owner, token: token})))))

(define-private (add-stx (owner principal) (amt uint))
  (let ((prev (default-to u0 (get balance (map-get? stx-balances {owner: owner})))))
    (map-set stx-balances {owner: owner} {balance: (+ prev amt)})
    (ok u1)))

(define-private (sub-stx (owner principal) (amt uint))
  (let ((prev (default-to u0 (get balance (map-get? stx-balances {owner: owner})))))
    (asserts! (>= prev amt) ERR-INSUFFICIENT)
    (map-set stx-balances {owner: owner} {balance: (- prev amt)})
    (ok u1)))

(define-private (add-ft (owner principal) (token principal) (amt uint))
  (let ((key {owner: owner, token: token})
        (prev (default-to u0 (get balance (map-get? ft-balances key)))))
    (map-set ft-balances key {balance: (+ prev amt)})
    (ok u1)))

(define-private (sub-ft (owner principal) (token principal) (amt uint))
  (let ((key {owner: owner, token: token})
        (prev (default-to u0 (get balance (map-get? ft-balances key)))))
    (asserts! (>= prev amt) ERR-INSUFFICIENT)
    (map-set ft-balances key {balance: (- prev amt)})
    (ok u1)))


;; -------------------------
;; Contract on-chain balance helpers
;; -------------------------
;; returns contract's STX balance
(define-read-only (contract-stx-balance)
  (ok (stx-get-balance (as-contract tx-sender))))

;; -------------------------
;; STX deposit & withdrawal flow
;; -------------------------
;; Flow:
;; 1) User sends STX to contract principal (separate blockchain transfer)
;; 2) User calls contract-call? credit-stx to credit their vault for the observed incoming delta
;; 3) User can withdraw via withdraw-stx

(define-public (credit-stx)
  (begin
    (asserts! (not (var-get paused)) ERR-PAUSED)
    (let ((current (stx-get-balance (as-contract tx-sender)))
          (accounted (default-to u0 (get amount (map-get? accounted-stx {dummy: true})))))
      (let ((delta (if (>= current accounted) (- current accounted) u0)))
        (asserts! (> delta u0) ERR-NO-DEPOSIT)
        (unwrap! (add-stx tx-sender delta) ERR-UNAUTHORIZED)
        (map-set accounted-stx {dummy: true} {amount: current})
        (ok delta)))))

(define-public (withdraw-stx (amount uint))
  (begin
    (asserts! (not (var-get paused)) ERR-PAUSED)
    (asserts! (> amount u0) ERR-NOT-POSITIVE)
    (unwrap! (sub-stx tx-sender amount) ERR-INSUFFICIENT)
    (match (stx-transfer? amount (as-contract tx-sender) tx-sender)
      success (begin
                ;; update accounted total to new on-chain balance after transfer
                (let ((new-balance (stx-get-balance (as-contract tx-sender))))
                  (map-set accounted-stx {dummy: true} {amount: new-balance})
                  (ok amount)))
      error (err error))))

;; -------------------------
;; FT deposit & withdrawal flow (SIP-010)
;; -------------------------
;; Flow:
;; 1) User calls the token contract's `transfer` to send tokens to this contract principal
;; 2) User calls `credit-ft <token-contract>` to credit their vault
;; 3) Withdraw with `withdraw-ft <token> <amount>`

(define-public (credit-ft (token <ft-trait>))
  (begin
    (asserts! (not (var-get paused)) ERR-PAUSED)
    (let ((contract-addr (as-contract tx-sender))
          (token-contract (contract-of token)))
      (let ((balance-response (contract-call? token get-balance contract-addr)))
        (asserts! (is-ok balance-response) ERR-UNAUTHORIZED)
        (let ((current-balance (unwrap! balance-response ERR-UNAUTHORIZED))
              (prev (default-to u0 (get balance (map-get? ft-balances {owner: tx-sender, token: token-contract}))))
              (delta (if (>= current-balance prev) (- current-balance prev) u0)))
          (asserts! (> delta u0) ERR-NO-DEPOSIT)
          (unwrap! (add-ft tx-sender token-contract delta) ERR-UNAUTHORIZED)
          (ok delta))))))

(define-public (withdraw-ft (token <ft-trait>) (amount uint))
  (begin
    (asserts! (not (var-get paused)) ERR-PAUSED)
    (asserts! (> amount u0) ERR-NOT-POSITIVE)
    (let
      (
        (token-contract (contract-of token))
        (owner tx-sender)
      )
      (try! (sub-ft owner token-contract amount))
      (let ((transfer-result (contract-call? token transfer amount (as-contract tx-sender) owner none)))
        (asserts! (is-ok transfer-result) ERR-UNAUTHORIZED)
        (ok amount)))))

;; -------------------------
;; Read-only overview
;; -------------------------
(define-public (vault-overview (owner principal) (token <ft-trait>))
  (ok {
    stx: (default-to u0 (get balance (map-get? stx-balances {owner: owner}))),
    ft: (default-to u0 (get balance (map-get? ft-balances {owner: owner, token: (contract-of token)})))
  }))

;; -------------------------
;; Admin emergency withdrawals
;; -------------------------
(define-public (admin-withdraw-stx (to principal) (amount uint))
  (begin
    (try! (assert-admin))
    (asserts! (> amount u0) ERR-NOT-POSITIVE)
    (let
      ((contract-addr (as-contract tx-sender))
       (transfer-result (stx-transfer? amount contract-addr to)))
      (asserts! (is-ok transfer-result) ERR-UNAUTHORIZED)
      (ok true))))

(define-public (admin-withdraw-ft (token <ft-trait>) (to principal) (amount uint))
  (begin
    (try! (assert-admin))
    (asserts! (> amount u0) ERR-NOT-POSITIVE)
    (let
      ((contract-addr (as-contract tx-sender))
       (transfer-result (contract-call? token transfer amount contract-addr to none)))
      (asserts! (is-ok transfer-result) ERR-UNAUTHORIZED)
      (ok true))))


