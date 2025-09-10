;; Loyalty Gift Card System Contract
;; Allows users to create gift cards using their loyalty points and send them to other users

;; Error constants
(define-constant err-owner-only (err u600))
(define-constant err-not-found (err u601))
(define-constant err-unauthorized (err u602))
(define-constant err-invalid-amount (err u604))
(define-constant err-already-claimed (err u605))
(define-constant err-expired (err u606))
(define-constant err-self-gift (err u607))
(define-constant err-invalid-expiry (err u608))
(define-constant err-cannot-cancel (err u609))

;; Contract owner
(define-constant contract-owner tx-sender)

;; Configuration variables
(define-data-var min-gift-amount uint u100)
(define-data-var max-gift-validity uint u8760)
(define-data-var gift-fee-percentage uint u2)

;; Gift card tracking
(define-data-var gift-card-nonce uint u0)
(define-data-var total-gift-cards-created uint u0)
(define-data-var total-gift-value uint u0)
(define-data-var total-gifts-claimed uint u0)

;; Gift card data structure
(define-map gift-cards
  { gift-id: uint }
  {
    sender: principal,
    recipient: principal,
    points-amount: uint,
    message: (string-ascii 200),
    created-at: uint,
    expires-at: uint,
    status: (string-ascii 20),
    claimed-at: (optional uint)
  }
)

;; User gift statistics
(define-map user-gift-stats
  { user: principal }
  {
    gifts-sent: uint,
    gifts-received: uint,
    total-sent-value: uint,
    total-received-value: uint
  }
)

;; Locked points for active gift cards
(define-map locked-points
  { user: principal }
  { locked-amount: uint }
)

;; Read-only functions
(define-read-only (get-gift-card (gift-id uint))
  (map-get? gift-cards { gift-id: gift-id })
)

(define-read-only (get-user-gift-stats (user principal))
  (default-to
    { gifts-sent: u0, gifts-received: u0, total-sent-value: u0, total-received-value: u0 }
    (map-get? user-gift-stats { user: user })
  )
)

(define-read-only (get-locked-points (user principal))
  (get locked-amount (default-to { locked-amount: u0 } (map-get? locked-points { user: user })))
)

(define-read-only (get-system-stats)
  {
    total-gift-cards: (var-get total-gift-cards-created),
    total-gift-value: (var-get total-gift-value),
    total-claimed: (var-get total-gifts-claimed),
    min-gift-amount: (var-get min-gift-amount),
    max-validity: (var-get max-gift-validity),
    gift-fee: (var-get gift-fee-percentage)
  }
)

(define-read-only (calculate-gift-fee (amount uint))
  (/ (* amount (var-get gift-fee-percentage)) u100)
)

(define-read-only (is-gift-active (gift-id uint))
  (match (get-gift-card gift-id)
    some gift-data (and
      (is-eq (get status gift-data) "active")
      (< stacks-block-height (get expires-at gift-data))
    )
    false
  )
)

;; Main gift card functions
(define-public (create-gift-card (recipient principal) (points-amount uint) (message (string-ascii 200)) (validity-blocks uint))
  (let (
    (gift-id (+ (var-get gift-card-nonce) u1))
    (expires-at (+ stacks-block-height validity-blocks))
    (gift-fee (calculate-gift-fee points-amount))
    (total-cost (+ points-amount gift-fee))
    (sender-stats (get-user-gift-stats tx-sender))
    (sender-locked (get-locked-points tx-sender))
  )
    ;; Validations
    (asserts! (not (is-eq tx-sender recipient)) (err err-self-gift))
    (asserts! (>= points-amount (var-get min-gift-amount)) (err err-invalid-amount))
    (asserts! (<= validity-blocks (var-get max-gift-validity)) (err err-invalid-expiry))
    (asserts! (> validity-blocks u0) (err err-invalid-expiry))
    
    ;; Create the gift card
    (map-set gift-cards
      { gift-id: gift-id }
      {
        sender: tx-sender,
        recipient: recipient,
        points-amount: points-amount,
        message: message,
        created-at: stacks-block-height,
        expires-at: expires-at,
        status: "active",
        claimed-at: none
      }
    )
    
    ;; Update system statistics
    (var-set gift-card-nonce gift-id)
    (var-set total-gift-cards-created (+ (var-get total-gift-cards-created) u1))
    (var-set total-gift-value (+ (var-get total-gift-value) points-amount))
    
    (ok gift-id)
  )
)

(define-public (claim-gift-card (gift-id uint))
  (let (
    (gift-card (unwrap! (get-gift-card gift-id) (err err-not-found)))
  )
    ;; Validations
    (asserts! (is-eq tx-sender (get recipient gift-card)) (err err-unauthorized))
    (asserts! (is-eq (get status gift-card) "active") (err err-already-claimed))
    (asserts! (< stacks-block-height (get expires-at gift-card)) (err err-expired))
    
    ;; Update gift card status
    (map-set gift-cards
      { gift-id: gift-id }
      (merge gift-card {
        status: "claimed",
        claimed-at: (some stacks-block-height)
      })
    )
    
    ;; Update system statistics
    (var-set total-gifts-claimed (+ (var-get total-gifts-claimed) u1))
    
    (ok (get points-amount gift-card))
  )
)

(define-public (update-gift-settings (min-amount uint) (max-validity uint) (fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (asserts! (> min-amount u0) (err err-invalid-amount))
    (asserts! (> max-validity u0) (err err-invalid-amount))
    (asserts! (<= fee-percentage u10) (err err-invalid-amount))
    
    (var-set min-gift-amount min-amount)
    (var-set max-gift-validity max-validity)
    (var-set gift-fee-percentage fee-percentage)
    (ok true)
  )
)
