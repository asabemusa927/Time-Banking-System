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