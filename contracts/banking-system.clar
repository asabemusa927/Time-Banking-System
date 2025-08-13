(define-data-var admin principal tx-sender)

(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_ALREADY_EXISTS (err u104))
(define-constant ERR_INVALID_SERVICE (err u105))
(define-constant ERR_INVALID_TRANSACTION (err u106))


(define-map users
  { user: principal }
  { balance: uint, reputation: uint, active: bool, tokens: uint }
)

(define-map services
  { id: uint }
  {
    provider: principal,
    name: (string-ascii 50),
    description: (string-ascii 200),
    rate: uint,
    active: bool
  }
)

(define-map transactions
  { id: uint }
  {
    provider: principal,
    consumer: principal,
    service-id: uint,
    hours: uint,
    status: (string-ascii 20),
    timestamp: uint
  }
)

(define-data-var service-counter uint u0)
(define-data-var transaction-counter uint u0)

(define-read-only (get-admin)
  (var-get admin)
)

(define-read-only (get-user-balance (user principal))
  (default-to u0 (get balance (map-get? users { user: user })))
)

(define-read-only (get-user-reputation (user principal))
  (default-to u0 (get reputation (map-get? users { user: user })))
)

(define-read-only (get-user (user principal))
  (map-get? users { user: user })
)

(define-read-only (get-service (id uint))
  (map-get? services { id: id })
)

(define-read-only (get-transaction (id uint))
  (map-get? transactions { id: id })
)

(define-read-only (get-service-counter)
  (var-get service-counter)
)

(define-read-only (get-transaction-counter)
  (var-get transaction-counter)
)

(define-public (register-user)
  (let ((user tx-sender))
    (if (is-some (map-get? users { user: user }))
      ERR_ALREADY_EXISTS
      (begin
        (map-set users
          { user: user }
          { balance: u10, reputation: u0, active: true, tokens: u0 }
        )
        (ok true)
      )
    )
  )
)

(define-public (register-service (name (string-ascii 50)) (description (string-ascii 200)) (rate uint))
  (let (
    (user tx-sender)
    (service-id (+ (var-get service-counter) u1))
  )
    (if (is-none (map-get? users { user: user }))
      ERR_NOT_FOUND
      (begin
        (var-set service-counter service-id)
        (map-set services
          { id: service-id }
          {
            provider: user,
            name: name,
            description: description,
            rate: rate,
            active: true
          }
        )
        (ok service-id)
      )
    )
  )
)

(define-public (update-service (id uint) (name (string-ascii 50)) (description (string-ascii 200)) (rate uint) (active bool))
  (let ((service (map-get? services { id: id })))
    (if (is-none service)
      ERR_NOT_FOUND
      (if (not (is-eq tx-sender (get provider (unwrap-panic service))))
        ERR_UNAUTHORIZED
        (begin
          (map-set services
            { id: id }
            {
              provider: tx-sender,
              name: name,
              description: description,
              rate: rate,
              active: active
            }
          )
          (ok true)
        )
      )
    )
  )
)

(define-public (request-service (service-id uint) (hours uint))
  (let (
    (service (map-get? services { id: service-id }))
    (consumer tx-sender)
    (transaction-id (+ (var-get transaction-counter) u1))
  )
    (if (is-none service)
      ERR_INVALID_SERVICE
      (let (
        (provider (get provider (unwrap-panic service)))
        (rate (get rate (unwrap-panic service)))
        (total-cost (* hours rate))
      )
        (if (<= hours u0)
          ERR_INVALID_AMOUNT
          (if (< (get-user-balance consumer) total-cost)
            ERR_INSUFFICIENT_BALANCE
            (begin
              (var-set transaction-counter transaction-id)
              (map-set transactions
                { id: transaction-id }
                {
                  provider: provider,
                  consumer: consumer,
                  service-id: service-id,
                  hours: hours,
                  status: "pending",
                  timestamp: stacks-block-height
                }
              )
              (ok transaction-id)
            )
          )
        )
      )
    )
  )
)

(define-public (approve-service-request (transaction-id uint))
  (let ((tx (map-get? transactions { id: transaction-id })))
    (if (is-none tx)
      ERR_NOT_FOUND
      (let ((transaction (unwrap-panic tx)))
        (if (not (is-eq tx-sender (get provider transaction)))
          ERR_UNAUTHORIZED
          (if (not (is-eq (get status transaction) "pending"))
            ERR_INVALID_TRANSACTION
            (begin
              (map-set transactions
                { id: transaction-id }
                (merge transaction { status: "approved" })
              )
              (ok true)
            )
          )
        )
      )
    )
  )
)

(define-public (complete-service (transaction-id uint))
  (let ((tx (map-get? transactions { id: transaction-id })))
    (if (is-none tx)
      ERR_NOT_FOUND
      (let (
        (transaction (unwrap-panic tx))
        (provider (get provider transaction))
        (consumer (get consumer transaction))
        (hours (get hours transaction))
        (service-id (get service-id transaction))
      )
        (if (not (is-eq tx-sender provider))
          ERR_UNAUTHORIZED
          (if (not (is-eq (get status transaction) "approved"))
            ERR_INVALID_TRANSACTION
            (let (
              (service (unwrap-panic (map-get? services { id: service-id })))
              (rate (get rate service))
              (total-cost (* hours rate))
              (consumer-data (unwrap-panic (map-get? users { user: consumer })))
              (provider-data (unwrap-panic (map-get? users { user: provider })))
            )
              (begin
                (map-set users
                  { user: consumer }
                  (merge consumer-data { 
                    balance: (- (get balance consumer-data) total-cost),
                    reputation: (+ (get reputation consumer-data) u1)
                  })
                )
                (map-set users
                  { user: provider }
                  (merge provider-data { 
                    balance: (+ (get balance provider-data) total-cost),
                    reputation: (+ (get reputation provider-data) u1)
                  })
                )
                (map-set transactions
                  { id: transaction-id }
                  (merge transaction { status: "completed" })
                )
                (ok true)
              )
            )
          )
        )
      )
    )
  )
)

(define-public (cancel-service-request (transaction-id uint))
  (let ((tx (map-get? transactions { id: transaction-id })))
    (if (is-none tx)
      ERR_NOT_FOUND
      (let ((transaction (unwrap-panic tx)))
        (if (and 
              (not (is-eq tx-sender (get consumer transaction)))
              (not (is-eq tx-sender (get provider transaction)))
            )
          ERR_UNAUTHORIZED
          (if (not (or 
                (is-eq (get status transaction) "pending")
                (is-eq (get status transaction) "approved")
              ))
            ERR_INVALID_TRANSACTION
            (begin
              (map-set transactions
                { id: transaction-id }
                (merge transaction { status: "cancelled" })
              )
              (ok true)
            )
          )
        )
      )
    )
  )
)

(define-public (set-admin (new-admin principal))
  (if (is-eq tx-sender (var-get admin))
    (begin
      (var-set admin new-admin)
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)



(define-constant ERR_INSUFFICIENT_TOKENS (err u107))
(define-constant ERR_INVALID_REWARD_TIER (err u108))


;; Add reward tiers map
(define-map reward-tiers
  { tier-id: uint }
  { 
    name: (string-ascii 50),
    token-cost: uint,
    discount-percentage: uint,
    active: bool
  }
)

;; Initialize reward tiers
(define-public (initialize-reward-tiers)
  (if (is-eq tx-sender (var-get admin))
    (begin
      (map-set reward-tiers { tier-id: u1 } { name: "Bronze", token-cost: u10, discount-percentage: u5, active: true })
      (map-set reward-tiers { tier-id: u2 } { name: "Silver", token-cost: u25, discount-percentage: u10, active: true })
      (map-set reward-tiers { tier-id: u3 } { name: "Gold", token-cost: u50, discount-percentage: u20, active: true })
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)



;; Modify complete-service to award tokens
(define-public (complete-service-new (transaction-id uint))
  (let ((tx (map-get? transactions { id: transaction-id })))
    (if (is-none tx)
      ERR_NOT_FOUND
      (let (
        (transaction (unwrap-panic tx))
        (provider (get provider transaction))
        (consumer (get consumer transaction))
        (hours (get hours transaction))
        (service-id (get service-id transaction))
      )
        (if (not (is-eq tx-sender provider))
          ERR_UNAUTHORIZED
          (if (not (is-eq (get status transaction) "approved"))
            ERR_INVALID_TRANSACTION
            (let (
              (service (unwrap-panic (map-get? services { id: service-id })))
              (rate (get rate service))
              (total-cost (* hours rate))
              (consumer-data (unwrap-panic (map-get? users { user: consumer })))
              (provider-data (unwrap-panic (map-get? users { user: provider })))
              (tokens-earned (/ total-cost u10))
            )
              (begin
                (map-set users
                  { user: consumer }
                  (merge consumer-data { 
                    balance: (- (get balance consumer-data) total-cost),
                    reputation: (+ (get reputation consumer-data) u1),
                    tokens: (+ (get tokens consumer-data) tokens-earned)
                  })
                )
                (map-set users
                  { user: provider }
                  (merge provider-data { 
                    balance: (+ (get balance provider-data) total-cost),
                    reputation: (+ (get reputation provider-data) u1),
                    tokens: (+ (get tokens provider-data) tokens-earned)
                  })
                )
                (map-set transactions
                  { id: transaction-id }
                  (merge transaction { status: "completed" })
                )
                (ok true)
              )
            )
          )
        )
      )
    )
  )
)

;; Get user tokens
(define-read-only (get-user-tokens (user principal))
  (default-to u0 (get tokens (map-get? users { user: user })))
)

;; Get reward tier details
(define-read-only (get-reward-tier (tier-id uint))
  (map-get? reward-tiers { tier-id: tier-id })
)

;; Redeem tokens for a discount
(define-public (redeem-reward (tier-id uint))
  (let (
    (user tx-sender)
    (user-data (map-get? users { user: user }))
    (tier (map-get? reward-tiers { tier-id: tier-id }))
  )
    (if (is-none user-data)
      ERR_NOT_FOUND
      (if (is-none tier)
        ERR_INVALID_REWARD_TIER
        (let (
          (user-tokens (get tokens (unwrap-panic user-data)))
          (token-cost (get token-cost (unwrap-panic tier)))
        )
          (if (< user-tokens token-cost)
            ERR_INSUFFICIENT_TOKENS
            (begin
              (map-set users
                { user: user }
                (merge (unwrap-panic user-data) 
                  { tokens: (- user-tokens token-cost) }
                )
              )
              (ok tier-id)
            )
          )
        )
      )
    )
  )
)


(define-public (get-reward-tier-discount (tier-id uint))
  (let ((tier (map-get? reward-tiers { tier-id: tier-id })))
    (if (is-none tier)
      ERR_INVALID_REWARD_TIER
      (ok (get discount-percentage (unwrap-panic tier)))
    )
  )
)
(define-public (get-reward-tier-name (tier-id uint))
  (let ((tier (map-get? reward-tiers { tier-id: tier-id })))
    (if (is-none tier)
      ERR_INVALID_REWARD_TIER
      (ok (get name (unwrap-panic tier)))
    )
  )
)
(define-public (get-reward-tier-active (tier-id uint))
  (let ((tier (map-get? reward-tiers { tier-id: tier-id })))
    (if (is-none tier)
      ERR_INVALID_REWARD_TIER
      (ok (get active (unwrap-panic tier)))
    )
  )
)
(define-public (set-reward-tier-active (tier-id uint) (active bool))
  (if (is-eq tx-sender (var-get admin))
    (begin
      (map-set reward-tiers
        { tier-id: tier-id }
        { name: "Bronze", token-cost: u10, discount-percentage: u5, active: active }
      )
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)


(define-constant ERR_ALREADY_DISPUTED (err u109))
(define-constant ERR_NOT_DISPUTED (err u110))
(define-constant ERR_DISPUTE_CLOSED (err u111))

(define-map disputes
  { transaction-id: uint }
  {
    initiator: principal,
    reason: (string-ascii 200),
    status: (string-ascii 20),
    resolution: (string-ascii 200),
    timestamp: uint
  }
)

;; Create a dispute
(define-public (create-dispute (transaction-id uint) (reason (string-ascii 200)))
  (let (
    (tx (map-get? transactions { id: transaction-id }))
    (existing-dispute (map-get? disputes { transaction-id: transaction-id }))
  )
    (if (is-none tx)
      ERR_NOT_FOUND
      (if (is-some existing-dispute)
        ERR_ALREADY_DISPUTED
        (let (
          (transaction (unwrap-panic tx))
          (user tx-sender)
        )
          (if (and 
                (not (is-eq user (get consumer transaction)))
                (not (is-eq user (get provider transaction)))
              )
            ERR_UNAUTHORIZED
            (if (not (or 
                  (is-eq (get status transaction) "approved")
                  (is-eq (get status transaction) "completed")
                ))
              ERR_INVALID_TRANSACTION
              (begin
                (map-set disputes
                  { transaction-id: transaction-id }
                  {
                    initiator: user,
                    reason: reason,
                    status: "open",
                    resolution: "",
                    timestamp: stacks-block-height
                  }
                )
                (map-set transactions
                  { id: transaction-id }
                  (merge transaction { status: "disputed" })
                )
                (ok true)
              )
            )
          )
        )
      )
    )
  )
)

;; Get dispute details
(define-read-only (get-dispute (transaction-id uint))
  (map-get? disputes { transaction-id: transaction-id })
)

;; Resolve a dispute (admin only)
(define-public (resolve-dispute (transaction-id uint) (resolution (string-ascii 200)) (in-favor-of principal))
  (let (
    (dispute (map-get? disputes { transaction-id: transaction-id }))
    (tx (map-get? transactions { id: transaction-id }))
  )
    (if (not (is-eq tx-sender (var-get admin)))
      ERR_UNAUTHORIZED
      (if (is-none dispute)
        ERR_NOT_FOUND
        (if (is-none tx)
          ERR_NOT_FOUND
          (let (
            (dispute-data (unwrap-panic dispute))
            (transaction (unwrap-panic tx))
            (provider (get provider transaction))
            (consumer (get consumer transaction))
          )
            (if (not (is-eq (get status dispute-data) "open"))
              ERR_DISPUTE_CLOSED
              (if (and 
                    (not (is-eq in-favor-of provider))
                    (not (is-eq in-favor-of consumer))
                  )
                ERR_UNAUTHORIZED
                (begin
                  (map-set disputes
                    { transaction-id: transaction-id }
                    (merge dispute-data 
                      { 
                        status: "resolved",
                        resolution: resolution
                      }
                    )
                  )
                  
                  ;; If resolved in favor of consumer, refund the payment
                  (if (is-eq in-favor-of consumer)
                    (let (
                      (service (unwrap-panic (map-get? services { id: (get service-id transaction) })))
                      (rate (get rate service))
                      (total-cost (* (get hours transaction) rate))
                      (consumer-data (unwrap-panic (map-get? users { user: consumer })))
                      (provider-data (unwrap-panic (map-get? users { user: provider })))
                    )
                      (begin
                        (map-set users
                          { user: consumer }
                          (merge consumer-data { 
                            balance: (+ (get balance consumer-data) total-cost)
                          })
                        )
                        (map-set users
                          { user: provider }
                          (merge provider-data { 
                            balance: (- (get balance provider-data) total-cost)
                          })
                        )
                        (map-set transactions
                          { id: transaction-id }
                          (merge transaction { status: "refunded" })
                        )
                        (ok true)
                      )
                    )
                    (begin
                      (map-set transactions
                        { id: transaction-id }
                        (merge transaction { status: "completed" })
                      )
                      (ok true)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;; Cancel a dispute (can only be done by the initiator)
(define-public (cancel-dispute (transaction-id uint))
  (let (
    (dispute (map-get? disputes { transaction-id: transaction-id }))
    (tx (map-get? transactions { id: transaction-id }))
  )
    (if (is-none dispute)
      ERR_NOT_DISPUTED
      (let (
        (dispute-data (unwrap-panic dispute))
        (transaction (unwrap-panic tx))
      )
        (if (not (is-eq tx-sender (get initiator dispute-data)))
          ERR_UNAUTHORIZED
          (if (not (is-eq (get status dispute-data) "open"))
            ERR_DISPUTE_CLOSED
            (begin
              (map-set disputes
                { transaction-id: transaction-id }
                (merge dispute-data { status: "cancelled" })
              )
              (map-set transactions
                { id: transaction-id }
                (merge transaction { status: "approved" })
              )
              (ok true)
            )
          )
        )
      )
    )
  )
)


(define-constant ERR_INVALID_MILESTONE (err u112))
(define-constant ERR_MILESTONE_COMPLETED (err u113))
(define-constant ERR_ESCROW_NOT_FOUND (err u114))
(define-constant ERR_ALL_MILESTONES_COMPLETED (err u115))
(define-constant ERR_ALREADY_REVIEWED (err u116))
(define-constant ERR_INVALID_RATING (err u117))
(define-constant ERR_CANNOT_REVIEW_OWN_SERVICE (err u118))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u119))
(define-constant ERR_SUBSCRIPTION_ALREADY_EXISTS (err u120))
(define-constant ERR_INVALID_FREQUENCY (err u121))
(define-constant ERR_SUBSCRIPTION_NOT_ACTIVE (err u122))
(define-constant ERR_SUBSCRIPTION_PAUSED (err u123))
(define-constant ERR_PAYMENT_FAILED (err u124))

(define-map escrows
  { escrow-id: uint }
  {
    transaction-id: uint,
    provider: principal,
    consumer: principal,
    total-amount: uint,
    released-amount: uint,
    milestone-count: uint,
    completed-milestones: uint,
    status: (string-ascii 20),
    created-at: uint
  }
)

(define-map milestones
  { escrow-id: uint, milestone-id: uint }
  {
    description: (string-ascii 200),
    amount: uint,
    completed: bool,
    completed-at: uint,
    approved-by-consumer: bool
  }
)

(define-data-var escrow-counter uint u0)

(define-map reviews
  { review-id: uint }
  {
    transaction-id: uint,
    service-id: uint,
    reviewer: principal,
    provider: principal,
    rating: uint,
    comment: (string-ascii 500),
    timestamp: uint
  }
)

(define-data-var review-counter uint u0)

;; Subscription system for recurring services
(define-map subscriptions
  { subscription-id: uint }
  {
    service-id: uint,
    subscriber: principal,
    provider: principal,
    frequency-blocks: uint, ;; blocks between payments
    amount-per-payment: uint,
    total-payments: uint,
    payments-made: uint,
    status: (string-ascii 20), ;; active, paused, cancelled, completed
    next-payment-block: uint,
    created-at: uint,
    last-payment-block: uint
  }
)

(define-data-var subscription-counter uint u0)

(define-public (create-escrow (transaction-id uint) (milestone-descriptions (list 10 (string-ascii 200))) (milestone-amounts (list 10 uint)))
  (let (
    (tx (map-get? transactions { id: transaction-id }))
    (escrow-id (+ (var-get escrow-counter) u1))
    (consumer tx-sender)
  )
    (if (is-none tx)
      ERR_NOT_FOUND
      (let (
        (transaction (unwrap-panic tx))
        (provider (get provider transaction))
        (service-id (get service-id transaction))
        (hours (get hours transaction))
      )
        (if (not (is-eq consumer (get consumer transaction)))
          ERR_UNAUTHORIZED
          (if (not (is-eq (get status transaction) "approved"))
            ERR_INVALID_TRANSACTION
            (let (
              (service (unwrap-panic (map-get? services { id: service-id })))
              (rate (get rate service))
              (total-cost (* hours rate))
              (milestone-count (len milestone-descriptions))
              (total-milestone-amount (fold + milestone-amounts u0))
            )
              (if (not (is-eq total-cost total-milestone-amount))
                ERR_INVALID_AMOUNT
                (begin
                  (var-set escrow-counter escrow-id)
                  (map-set escrows
                    { escrow-id: escrow-id }
                    {
                      transaction-id: transaction-id,
                      provider: provider,
                      consumer: consumer,
                      total-amount: total-cost,
                      released-amount: u0,
                      milestone-count: milestone-count,
                      completed-milestones: u0,
                      status: "active",
                      created-at: stacks-block-height
                    }
                  )
                  ;; (setup-milestones escrow-id milestone-descriptions milestone-amounts u1)
                  (map-set transactions
                    { id: transaction-id }
                    (merge transaction { status: "escrowed" })
                  )
                  (ok escrow-id)
                )
              )
            )
          )
        )
      )
    )
  )
)



(define-public (complete-milestone (escrow-id uint) (milestone-id uint))
  (let (
    (escrow (map-get? escrows { escrow-id: escrow-id }))
    (milestone (map-get? milestones { escrow-id: escrow-id, milestone-id: milestone-id }))
  )
    (if (is-none escrow)
      ERR_ESCROW_NOT_FOUND
      (if (is-none milestone)
        ERR_INVALID_MILESTONE
        (let (
          (escrow-data (unwrap-panic escrow))
          (milestone-data (unwrap-panic milestone))
          (provider (get provider escrow-data))
        )
          (if (not (is-eq tx-sender provider))
            ERR_UNAUTHORIZED
            (if (get completed milestone-data)
              ERR_MILESTONE_COMPLETED
              (begin
                (map-set milestones
                  { escrow-id: escrow-id, milestone-id: milestone-id }
                  (merge milestone-data 
                    { 
                      completed: true,
                      completed-at: stacks-block-height
                    }
                  )
                )
                (ok true)
              )
            )
          )
        )
      )
    )
  )
)

(define-public (approve-milestone (escrow-id uint) (milestone-id uint))
  (let (
    (escrow (map-get? escrows { escrow-id: escrow-id }))
    (milestone (map-get? milestones { escrow-id: escrow-id, milestone-id: milestone-id }))
  )
    (if (is-none escrow)
      ERR_ESCROW_NOT_FOUND
      (if (is-none milestone)
        ERR_INVALID_MILESTONE
        (let (
          (escrow-data (unwrap-panic escrow))
          (milestone-data (unwrap-panic milestone))
          (consumer (get consumer escrow-data))
          (provider (get provider escrow-data))
        )
          (if (not (is-eq tx-sender consumer))
            ERR_UNAUTHORIZED
            (if (not (get completed milestone-data))
              ERR_INVALID_MILESTONE
              (if (get approved-by-consumer milestone-data)
                ERR_MILESTONE_COMPLETED
                (let (
                  (milestone-amount (get amount milestone-data))
                  (provider-data (unwrap-panic (map-get? users { user: provider })))
                  (consumer-data (unwrap-panic (map-get? users { user: consumer })))
                  (new-released-amount (+ (get released-amount escrow-data) milestone-amount))
                  (new-completed-milestones (+ (get completed-milestones escrow-data) u1))
                )
                  (begin
                    (map-set milestones
                      { escrow-id: escrow-id, milestone-id: milestone-id }
                      (merge milestone-data { approved-by-consumer: true })
                    )
                    (map-set users
                      { user: provider }
                      (merge provider-data { 
                        balance: (+ (get balance provider-data) milestone-amount),
                        reputation: (+ (get reputation provider-data) u1)
                      })
                    )
                    (map-set users
                      { user: consumer }
                      (merge consumer-data { 
                        balance: (- (get balance consumer-data) milestone-amount)
                      })
                    )
                    (map-set escrows
                      { escrow-id: escrow-id }
                      (merge escrow-data 
                        { 
                          released-amount: new-released-amount,
                          completed-milestones: new-completed-milestones,
                          status: (if (is-eq new-completed-milestones (get milestone-count escrow-data)) "completed" "active")
                        }
                      )
                    )
                    (ok true)
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

(define-public (cancel-escrow (escrow-id uint))
  (let (
    (escrow (map-get? escrows { escrow-id: escrow-id }))
  )
    (if (is-none escrow)
      ERR_ESCROW_NOT_FOUND
      (let (
        (escrow-data (unwrap-panic escrow))
        (consumer (get consumer escrow-data))
        (provider (get provider escrow-data))
        (transaction-id (get transaction-id escrow-data))
      )
        (if (and 
              (not (is-eq tx-sender consumer))
              (not (is-eq tx-sender provider))
            )
          ERR_UNAUTHORIZED
          (if (not (is-eq (get status escrow-data) "active"))
            ERR_INVALID_TRANSACTION
            (begin
              (map-set escrows
                { escrow-id: escrow-id }
                (merge escrow-data { status: "cancelled" })
              )
              (map-set transactions
                { id: transaction-id }
                { 
                  provider: provider,
                  consumer: consumer,
                  service-id: (get service-id (unwrap-panic (map-get? transactions { id: transaction-id }))),
                  hours: (get hours (unwrap-panic (map-get? transactions { id: transaction-id }))),
                  status: "cancelled",
                  timestamp: (get timestamp (unwrap-panic (map-get? transactions { id: transaction-id })))
                }
              )
              (ok true)
            )
          )
        )
      )
    )
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows { escrow-id: escrow-id })
)

(define-read-only (get-milestone (escrow-id uint) (milestone-id uint))
  (map-get? milestones { escrow-id: escrow-id, milestone-id: milestone-id })
)

(define-read-only (get-escrow-counter)
  (var-get escrow-counter)
)

(define-read-only (get-escrow-progress (escrow-id uint))
  (let ((escrow (map-get? escrows { escrow-id: escrow-id })))
    (if (is-none escrow)
      (err "Escrow not found")
      (let ((escrow-data (unwrap-panic escrow)))
        (ok {
          completed: (get completed-milestones escrow-data),
          total: (get milestone-count escrow-data),
          released-amount: (get released-amount escrow-data),
          total-amount: (get total-amount escrow-data)
        })
      )
    )
  )
)

(define-public (submit-review (transaction-id uint) (rating uint) (comment (string-ascii 500)))
  (let (
    (tx (map-get? transactions { id: transaction-id }))
    (reviewer tx-sender)
    (review-id (+ (var-get review-counter) u1))
  )
    (if (is-none tx)
      ERR_NOT_FOUND
      (let (
        (transaction (unwrap-panic tx))
        (provider (get provider transaction))
        (consumer (get consumer transaction))
        (service-id (get service-id transaction))
      )
        (if (not (is-eq reviewer consumer))
          ERR_UNAUTHORIZED
          (if (is-eq reviewer provider)
            ERR_CANNOT_REVIEW_OWN_SERVICE
            (if (not (is-eq (get status transaction) "completed"))
              ERR_INVALID_TRANSACTION
              (if (or (< rating u1) (> rating u5))
                ERR_INVALID_RATING
                (begin
                  (var-set review-counter review-id)
                  (map-set reviews
                    { review-id: review-id }
                    {
                      transaction-id: transaction-id,
                      service-id: service-id,
                      reviewer: reviewer,
                      provider: provider,
                      rating: rating,
                      comment: comment,
                      timestamp: stacks-block-height
                    }
                  )
                  (ok review-id)
                )
              )
            )
          )
        )
      )
    )
  )
)

(define-read-only (get-review (review-id uint))
  (map-get? reviews { review-id: review-id })
)

(define-read-only (get-review-counter)
  (var-get review-counter)
)

(define-read-only (get-service-rating (service-id uint))
  (let (
    (review-1 (get-review u1))
    (review-2 (get-review u2))
    (review-3 (get-review u3))
    (review-4 (get-review u4))
    (review-5 (get-review u5))
  )
    (ok (calculate-rating-for-service service-id review-1 review-2 review-3 review-4 review-5))
  )
)

(define-private (calculate-rating-for-service (service-id uint) (r1 (optional {transaction-id: uint, service-id: uint, reviewer: principal, provider: principal, rating: uint, comment: (string-ascii 500), timestamp: uint})) (r2 (optional {transaction-id: uint, service-id: uint, reviewer: principal, provider: principal, rating: uint, comment: (string-ascii 500), timestamp: uint})) (r3 (optional {transaction-id: uint, service-id: uint, reviewer: principal, provider: principal, rating: uint, comment: (string-ascii 500), timestamp: uint})) (r4 (optional {transaction-id: uint, service-id: uint, reviewer: principal, provider: principal, rating: uint, comment: (string-ascii 500), timestamp: uint})) (r5 (optional {transaction-id: uint, service-id: uint, reviewer: principal, provider: principal, rating: uint, comment: (string-ascii 500), timestamp: uint})))
  (let (
    (rating-1 (if (and (is-some r1) (is-eq (get service-id (unwrap-panic r1)) service-id)) (get rating (unwrap-panic r1)) u0))
    (rating-2 (if (and (is-some r2) (is-eq (get service-id (unwrap-panic r2)) service-id)) (get rating (unwrap-panic r2)) u0))
    (rating-3 (if (and (is-some r3) (is-eq (get service-id (unwrap-panic r3)) service-id)) (get rating (unwrap-panic r3)) u0))
    (rating-4 (if (and (is-some r4) (is-eq (get service-id (unwrap-panic r4)) service-id)) (get rating (unwrap-panic r4)) u0))
    (rating-5 (if (and (is-some r5) (is-eq (get service-id (unwrap-panic r5)) service-id)) (get rating (unwrap-panic r5)) u0))
    (total-rating (+ rating-1 (+ rating-2 (+ rating-3 (+ rating-4 rating-5)))))
    (count (+ (if (> rating-1 u0) u1 u0) (+ (if (> rating-2 u0) u1 u0) (+ (if (> rating-3 u0) u1 u0) (+ (if (> rating-4 u0) u1 u0) (if (> rating-5 u0) u1 u0))))))
  )
    (if (is-eq count u0) u0 (/ total-rating count))
  )
)

(define-read-only (get-service-review-count (service-id uint))
  (let (
    (review-1 (get-review u1))
    (review-2 (get-review u2))
    (review-3 (get-review u3))
    (review-4 (get-review u4))
    (review-5 (get-review u5))
  )
    (+ (if (and (is-some review-1) (is-eq (get service-id (unwrap-panic review-1)) service-id)) u1 u0)
       (+ (if (and (is-some review-2) (is-eq (get service-id (unwrap-panic review-2)) service-id)) u1 u0)
          (+ (if (and (is-some review-3) (is-eq (get service-id (unwrap-panic review-3)) service-id)) u1 u0)
             (+ (if (and (is-some review-4) (is-eq (get service-id (unwrap-panic review-4)) service-id)) u1 u0)
                (if (and (is-some review-5) (is-eq (get service-id (unwrap-panic review-5)) service-id)) u1 u0)
             )
          )
       )
    )
  )
)

;; Create a new subscription for recurring service payments
(define-public (create-subscription (service-id uint) (frequency-blocks uint) (total-payments uint))
  (let (
    (service (map-get? services { id: service-id }))
    (subscriber tx-sender)
    (subscription-id (+ (var-get subscription-counter) u1))
  )
    (if (is-none service)
      ERR_INVALID_SERVICE
      (let (
        (service-data (unwrap-panic service))
        (provider (get provider service-data))
        (rate (get rate service-data))
      )
        (if (not (get active service-data))
          ERR_INVALID_SERVICE
          (if (is-eq subscriber provider)
            ERR_CANNOT_REVIEW_OWN_SERVICE ;; reusing error for self-subscription
            (if (or (<= frequency-blocks u0) (<= total-payments u0))
              ERR_INVALID_FREQUENCY
              (if (< (get-user-balance subscriber) rate)
                ERR_INSUFFICIENT_BALANCE
                (begin
                  (var-set subscription-counter subscription-id)
                  (map-set subscriptions
                    { subscription-id: subscription-id }
                    {
                      service-id: service-id,
                      subscriber: subscriber,
                      provider: provider,
                      frequency-blocks: frequency-blocks,
                      amount-per-payment: rate,
                      total-payments: total-payments,
                      payments-made: u0,
                      status: "active",
                      next-payment-block: (+ stacks-block-height frequency-blocks),
                      created-at: stacks-block-height,
                      last-payment-block: u0
                    }
                  )
                  (ok subscription-id)
                )
              )
            )
          )
        )
      )
    )
  )
)

;; Execute subscription payment if due
(define-public (process-subscription-payment (subscription-id uint))
  (let (
    (subscription (map-get? subscriptions { subscription-id: subscription-id }))
  )
    (if (is-none subscription)
      ERR_SUBSCRIPTION_NOT_FOUND
      (let (
        (sub-data (unwrap-panic subscription))
        (subscriber (get subscriber sub-data))
        (provider (get provider sub-data))
        (amount (get amount-per-payment sub-data))
      )
        (if (not (is-eq (get status sub-data) "active"))
          ERR_SUBSCRIPTION_NOT_ACTIVE
          (if (< stacks-block-height (get next-payment-block sub-data))
            ERR_INVALID_TRANSACTION ;; payment not yet due
            (if (< (get-user-balance subscriber) amount)
              ERR_INSUFFICIENT_BALANCE
              (let (
                (subscriber-data (unwrap-panic (map-get? users { user: subscriber })))
                (provider-data (unwrap-panic (map-get? users { user: provider })))
                (new-payments-made (+ (get payments-made sub-data) u1))
                (new-status (if (>= new-payments-made (get total-payments sub-data)) "completed" "active"))
                (next-payment (if (is-eq new-status "completed") u0 (+ stacks-block-height (get frequency-blocks sub-data))))
              )
                (begin
                  ;; Transfer payment
                  (map-set users
                    { user: subscriber }
                    (merge subscriber-data { 
                      balance: (- (get balance subscriber-data) amount)
                    })
                  )
                  (map-set users
                    { user: provider }
                    (merge provider-data { 
                      balance: (+ (get balance provider-data) amount),
                      reputation: (+ (get reputation provider-data) u1)
                    })
                  )
                  ;; Update subscription
                  (map-set subscriptions
                    { subscription-id: subscription-id }
                    (merge sub-data {
                      payments-made: new-payments-made,
                      status: new-status,
                      next-payment-block: next-payment,
                      last-payment-block: stacks-block-height
                    })
                  )
                  (ok true)
                )
              )
            )
          )
        )
      )
    )
  )
)

;; Pause an active subscription
(define-public (pause-subscription (subscription-id uint))
  (let (
    (subscription (map-get? subscriptions { subscription-id: subscription-id }))
  )
    (if (is-none subscription)
      ERR_SUBSCRIPTION_NOT_FOUND
      (let (
        (sub-data (unwrap-panic subscription))
        (subscriber (get subscriber sub-data))
      )
        (if (not (is-eq tx-sender subscriber))
          ERR_UNAUTHORIZED
          (if (not (is-eq (get status sub-data) "active"))
            ERR_SUBSCRIPTION_NOT_ACTIVE
            (begin
              (map-set subscriptions
                { subscription-id: subscription-id }
                (merge sub-data { status: "paused" })
              )
              (ok true)
            )
          )
        )
      )
    )
  )
)

;; Resume a paused subscription
(define-public (resume-subscription (subscription-id uint))
  (let (
    (subscription (map-get? subscriptions { subscription-id: subscription-id }))
  )
    (if (is-none subscription)
      ERR_SUBSCRIPTION_NOT_FOUND
      (let (
        (sub-data (unwrap-panic subscription))
        (subscriber (get subscriber sub-data))
      )
        (if (not (is-eq tx-sender subscriber))
          ERR_UNAUTHORIZED
          (if (not (is-eq (get status sub-data) "paused"))
            ERR_SUBSCRIPTION_PAUSED
            (begin
              (map-set subscriptions
                { subscription-id: subscription-id }
                (merge sub-data { 
                  status: "active",
                  next-payment-block: (+ stacks-block-height (get frequency-blocks sub-data))
                })
              )
              (ok true)
            )
          )
        )
      )
    )
  )
)

;; Cancel a subscription (can be done by subscriber or provider)
(define-public (cancel-subscription (subscription-id uint))
  (let (
    (subscription (map-get? subscriptions { subscription-id: subscription-id }))
  )
    (if (is-none subscription)
      ERR_SUBSCRIPTION_NOT_FOUND
      (let (
        (sub-data (unwrap-panic subscription))
        (subscriber (get subscriber sub-data))
        (provider (get provider sub-data))
      )
        (if (and 
              (not (is-eq tx-sender subscriber))
              (not (is-eq tx-sender provider))
            )
          ERR_UNAUTHORIZED
          (if (is-eq (get status sub-data) "cancelled")
            ERR_INVALID_TRANSACTION
            (begin
              (map-set subscriptions
                { subscription-id: subscription-id }
                (merge sub-data { status: "cancelled" })
              )
              (ok true)
            )
          )
        )
      )
    )
  )
)

;; Get subscription details
(define-read-only (get-subscription (subscription-id uint))
  (map-get? subscriptions { subscription-id: subscription-id })
)

;; Get subscription counter
(define-read-only (get-subscription-counter)
  (var-get subscription-counter)
)

;; Check if subscription payment is due
(define-read-only (is-subscription-payment-due (subscription-id uint))
  (let (
    (subscription (map-get? subscriptions { subscription-id: subscription-id }))
  )
    (if (is-none subscription)
      (err "Subscription not found")
      (let (
        (sub-data (unwrap-panic subscription))
      )
        (ok (and 
          (is-eq (get status sub-data) "active")
          (>= stacks-block-height (get next-payment-block sub-data))
        ))
      )
    )
  )
)

;; Get active subscriptions count for a user (as subscriber)
(define-read-only (get-user-active-subscriptions (user principal))
  (let (
    (sub-1 (get-subscription u1))
    (sub-2 (get-subscription u2))
    (sub-3 (get-subscription u3))
    (sub-4 (get-subscription u4))
    (sub-5 (get-subscription u5))
  )
    (+ (if (and (is-some sub-1) (is-eq (get subscriber (unwrap-panic sub-1)) user) (is-eq (get status (unwrap-panic sub-1)) "active")) u1 u0)
       (+ (if (and (is-some sub-2) (is-eq (get subscriber (unwrap-panic sub-2)) user) (is-eq (get status (unwrap-panic sub-2)) "active")) u1 u0)
          (+ (if (and (is-some sub-3) (is-eq (get subscriber (unwrap-panic sub-3)) user) (is-eq (get status (unwrap-panic sub-3)) "active")) u1 u0)
             (+ (if (and (is-some sub-4) (is-eq (get subscriber (unwrap-panic sub-4)) user) (is-eq (get status (unwrap-panic sub-4)) "active")) u1 u0)
                (if (and (is-some sub-5) (is-eq (get subscriber (unwrap-panic sub-5)) user) (is-eq (get status (unwrap-panic sub-5)) "active")) u1 u0)
             )
          )
       )
    )
  )
)



