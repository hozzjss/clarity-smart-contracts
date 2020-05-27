(define-data-var licenser-address principal 'ST398K1WZTBVY6FE2YEHM6HP20VSNVSSPJTW0D53M)

(define-map licenses
  ((licensee principal))
  ((type int)
   (block int)))

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
 (map-get? licenses ((licensee licensee)))
)

(define-private (get-block-height)
  (to-int block-height)
)

(define-public (has-valid-license (licensee principal))
  (let ((
    ;; when = the current count of blocks in the block chain
    when (get-block-height))
    ;; license = either the license that is returned or none
    ;; as I understand the '?' after the function name let's us know
    ;; that it can return none 
    (license (get-license? licensee)))
    ;; define the registered license type
    ;; which can only be 
    ;; 1 for permanent
    ;; 2 for temporary
    ;; and there is no 0
    ;; check price list at the bottom
    ;; 0 says that it just doesn't exist
    (let ((license-type (default-to 0 (get type license)))
      ;; define the license block index at which the license would expire
      ;; to compare to the when which holds the current block height
      ;; which acts as a point in time
      ;; this would be irrelevant if the license is permanent 
      (license-block (default-to 0 (get block license))))
      ;; if the license is non existing say goodbye honey
      ;; you don't exist, therefore thou art invalid
      (if (is-eq license-type 0)
        (ok false)
        ;; if it's permanet skip every thing and just 
        ;; say "You're golden baby!"
        (if (is-eq  license-type 1)
          (ok true)
          (if (is-eq license-type 2)
          ;; check if it's expired by checking the two points in time
          ;; when = the block height after which the license expires
          ;; and needs to be renewed
          ;; if it's past time just say "You are not golden baby :("
            (ok (< when license-block))
            (ok false)
          )
        )
      )
    )
  )
)

(define-private (should-buy (type int) (duration int) (existing-type int) (existing-block int))
    ;; you can buy a second or third or millionth expiring license
    ;; but not more than one permanent license that wouldn't make sense
    ;; except in centralized apps xD
    (if (is-eq existing-type 1)
      false
      (if (is-eq existing-type 2)
        (if (is-eq type 1)
          true
          (if (is-eq type 2)
            ;; basically the duration would be how much blocks have been mined
            ;; after the block in which the license resides
            ;; the duration parameter is passed for conformity
            ;; I guess it's a surprise tool that will help us later
            (< existing-block (get-block-height))
            true
          )
        )
        true
      )
  )
)

(define-private (buy (type int) (duration int) (sender principal))
  (let ((existing-license? (get-license? sender))
        (price (get-price type)))
    ;; define the license price either the good flat fee for the permanent record
    ;; or the dynamic fee for the duration requested
    (let ((license-price
        (if (is-eq type 1)
          price
          (* price duration))))
      (let
      ;; buynow is a boolean that specifies whether
      ;; the license exists or not
      ;; let would make buynow available as a block -code block not stacks block- variable
      ;; so that it is available to the next if condition `if buynow`
        ((buynow (match existing-license?
          license (print (should-buy type duration (get type license) (get block license)))
          true)))
      ;; if the licesnse does not exist then commit to the purchase
        (if buynow
        ;; transferred is defined here to indicate whether the transfer was successful or not
          (let ((transferred (stx-transfer? (to-uint license-price) tx-sender (get-licenser))))
          ;; being availble as a response object is-ok checks on it and if it is okay
          ;; then we go ahead and persist the license in the contract's licenses map
            (if (is-ok transferred)
              (begin
                (map-set licenses ((licensee sender))
                  ((type type)
                  ;; the license will be valid until (duration) blocks have been mined
                  ;; would be irrelevant to duration if the license is permanent
                  (block (+ duration (get-block-height))))
                  )
                (ok license-price))
                
              payment-err)
          )
      ;; if the license exists throw license-exists err
          license-exists-err)
  )))
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
