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
  { balance: uint, reputation: uint, active: bool }
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
          { balance: u10, reputation: u0, active: true }
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
