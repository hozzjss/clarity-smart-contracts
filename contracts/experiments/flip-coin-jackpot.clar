;; A simple flip coin betting game.
;;
;; For more details see docs/flip-coin.md

;;
;; Flip coin contract
;;

(define-constant even-buffer 0x00020406080a0c0e10121416181a1c1e20222426282a2c2e30323436383a3c3e40424446484a4c4e50525456585a5c5e60626466686a6c6e70727476787a7c7e80828486888a8c8e90929496989a9c9ea0a2a4a6a8aaacaeb0b2b4b6b8babcbec0c2c4c6c8caccced0d2d4d6d8dadcdee0e2e4e6e8eaecee)

;; private functions

;; used in (fold) to get last item of a buffer
(define-private (last (item (buff 1)) (value  (buff 1)))
   item
)

;; used in (fold) to check if character is even
(define-private (is-even (item (buff 1)) (value-tuple {value: (buff 1), result: (buff 1)}))
  (let ((val (get value value-tuple)))
    (if (is-eq item val )
      {value: val, result: 0x00}
      {value: val, result: (get result value-tuple)}
    )
  )
)

;; public functions

;; check whether the character is even
(define-read-only (even (value (buff 1)))
  (is-eq (get result (fold is-even even-buffer {value: value, result: 0x01})) 0x00)
)

;; checks property of last byte of given buffer
;; returns true if the last byte of the hash is even
(define-read-only (is-last-even (hash (buff 32)) )
  (let ((last-value  (fold last hash "0")))
    (even last-value)
  )
)

;; flip coin by looking at the hash at the given block
;; returns true if the last byte of the hash is even
(define-read-only (flip-coin-at (height uint))
  (let ((hash (unwrap-panic (get-block-info? header-hash height))))
    (is-last-even hash)
  )
)


;; returns the random value based on the previous block
(define-read-only (flip-coin)
  (flip-coin-at (- block-height u1))
)

;;
;; Betting Game contract
;;

(define-constant default-amount u1000)
(define-constant err-bet-exists u10)

;; storage
(define-map gamblers ((height uint) (value bool)) ((principal principal) (amount uint)))
(define-map amounts ((height uint)) ((amount uint)))

(define-data-var pending-payout (optional uint) none)
(define-data-var jackpot uint u0)


;; public functions

;; returns how much stx were bet at the given block
(define-read-only (get-jackpot)
  (match (var-get pending-payout)
    height (match (get-optional-winner-at height)
              winner u0
              (+ (var-get jackpot) (get-amount-at height))
           )
    (var-get jackpot)
  )
)

;; returns how much stx were bet at the given block
(define-read-only (get-amount-at (height uint))
  (match (map-get? amounts ((height height)))
    amount (get amount amount)
    u0
  )
)

;; returns the winner at the given block. If there was no winner `(none)` is returned
(define-read-only (get-optional-winner-at (height uint))
  (match (map-get? gamblers ((height height) (value (flip-coin-at (+ height u1)))))
    gambler (some (get principal gambler))
    none
  )
)

;; pays the bet amount at the given block plus the jackpot
(define-private (payout (height (optional uint)))
 (match height
  some-height (if (<= block-height some-height)
    true
    (begin
      (match (get-optional-winner-at some-height)
        winner (begin
          (unwrap-panic (as-contract (stx-transfer? (+ (var-get jackpot) (get-amount-at some-height)) tx-sender winner)))
          (var-set jackpot u0)
          )
        (var-set jackpot (+ (var-get jackpot) (get-amount-at some-height)))
      )
      (var-set pending-payout none)
    ))
  true
 )
)

(define-private (update-game-after-payment (value bool) (amount uint))
  (begin
    (map-set amounts ((height block-height))  ((amount (+ (get-amount-at block-height) amount))))
    (var-set pending-payout (some block-height))
    (ok block-height)
  )
)

;; bet 1000 mSTX on the given value. Only one user can bet on that value for each block.
;; if payout needs to be done then this function call will do it (note that the caller
;; needs to provide corresponding post conditions)
(define-public (bet (value bool))
  (let ((amount default-amount))
    (begin
      (payout (var-get pending-payout))
      (if (map-insert gamblers ((height block-height) (value value)) ((amount amount) (principal tx-sender)))
        (match (stx-transfer? amount tx-sender (as-contract tx-sender))
          success (update-game-after-payment value amount)
          error (err error)
        )
        (err err-bet-exists)
      )
    )
  )
)

(impl-trait .flip-coin-tax-office.tax-office-trait)

(define-public (pay-tax (amount uint))
  (begin
    (var-set jackpot (+ (var-get jackpot) amount))
    (stx-transfer? amount tx-sender (as-contract tx-sender))
  )
)
