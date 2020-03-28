(define-data-var licenser-address principal 'SZ2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKQ9H6DPR)

(define-map licenses
  ((licensee principal))
  ((type int) (block int)))

(define-map price-list ((type int)) ((price int)))


(define-constant licenser-already-set-err (err 1))
(define-constant invalid-license-type-err (err 2))
(define-constant missing-licenser-err (err 3))
(define-constant payment-err (err 4))
(define-constant license-exists-err (err 5))

(define-private (get-licenser)
  (var-get licenser-address)
)

(define-private (get-price (t int))
  (match (map-get? price-list ((type t)))
    entry (get price entry)
    0
  )
)

(define-private (get-license? (licensee principal))
 (map-get? licenses {licensee: licensee})
)

(define-private (get-block-height)
  block-height
)

(define-fungible-token stacks)


(define-private (has-valid-license (licensee principal) (when int))
  (let ((license-type (default-to 0 (get type (map-get? licenses ((licensee licensee))))))
    (license-block (default-to 0 (get block (map-get? licenses ((licensee licensee)))))))
    (if (not (is-eq license-type 0))
      (if (is-eq  license-type 1)
        'true
        (if (is-eq license-type 2)
          (< when license-block)
          'false
         )
      )
      'false
    )
  )
)

(define-private (should-buy (type int) (duration int) (existing-type int) (existing-block int))
  (if (is-eq existing-type 1)
    'false
    (if (is-eq existing-type 2)
      (if (is-eq type 1)
        'true
        (if (is-eq type 2)
          (< existing-block (get-block-height))
          'true
        )
      )
      'true
    )
  )
)

(define-private (buy (type int) (duration int) (sender principal))
  (let ((existing-license (get-license? ((licensee sender)))
    (price (get-price type))
    (license-price
      (if (is-eq type 1)
        price
        (* price duration)))
    )
    (buynow (match existing-license
        license (should-buy type duration (get type license) (get block license))
        'false)))
    (if buynow
      (let ((transferred
        (ft-transfer? stacks (to-uint license-price) tx-sender licenser )))
        (if (is-ok transferred)
          (begin
            (map-set licenses ((licensee sender)) ((type type) (block (+ duration (get-block-height)))))
            (ok licensePrice))
          payment-err)
      )
      license-exists-err)
    )
)

(define-public (buy-non-expiring)
  (buy 1 0 tx-sender)
)

(define-public (buy-expiring (duration int))
  (buy 2 duration tx-sender)
)

(begin
  (map-insert price-list ((type 1)) ((price 100)))
  (map-insert price-list ((type 2)) ((price 1)))
)