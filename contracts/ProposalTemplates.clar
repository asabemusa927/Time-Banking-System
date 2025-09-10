;; Proposal Templates System - UpSignal Enhancement
;; Enables creation and management of standardized proposal templates

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_TEMPLATE_NOT_FOUND (err u201))
(define-constant ERR_TEMPLATE_INACTIVE (err u202))
(define-constant ERR_INSUFFICIENT_STAKE (err u203))
(define-constant ERR_ALREADY_VOTED (err u204))

;; Contract owner
(define-constant CONTRACT_OWNER tx-sender)

;; Configuration variables
(define-data-var template-counter uint u0)
(define-data-var min-template-stake uint u500000)

;; Template data structures
(define-map proposal-templates
  { template-id: uint }
  {
    name: (string-ascii 100),
    category: (string-ascii 50),
    description: (string-ascii 500),
    title-template: (string-ascii 200),
    description-template: (string-ascii 1000),
    creator: principal,
    created-at: uint,
    is-active: bool,
    usage-count: uint,
    approval-votes: uint,
    rejection-votes: uint
  }
)

;; Template voting records
(define-map template-votes
  { template-id: uint, voter: principal }
  {
    vote: bool,
    stake: uint,
    voted-at: uint
  }
)

;; User template usage tracking
(define-map user-template-usage
  { user: principal, template-id: uint }
  { usage-count: uint, last-used: uint }
)

;; Create a new proposal template
(define-public (create-template (name (string-ascii 100)) (category (string-ascii 50)) 
                               (description (string-ascii 500)) (title-template (string-ascii 200)) 
                               (description-template (string-ascii 1000)))
  (let (
    (template-id (+ (var-get template-counter) u1))
    (current-height stacks-block-height)
    (creator-stake (contract-call? .UpSignal get-user-stake tx-sender))
  )
    ;; Validate creator has sufficient stake
    (asserts! (>= creator-stake (var-get min-template-stake)) ERR_INSUFFICIENT_STAKE)
    
    ;; Create the template
    (map-set proposal-templates
      { template-id: template-id }
      {
        name: name,
        category: category,
        description: description,
        title-template: title-template,
        description-template: description-template,
        creator: tx-sender,
        created-at: current-height,
        is-active: true,
        usage-count: u0,
        approval-votes: u0,
        rejection-votes: u0
      }
    )
    
    ;; Update counter
    (var-set template-counter template-id)
    (ok template-id)
  )
)

;; Vote on a template to approve or reject it
(define-public (vote-on-template (template-id uint) (approve bool))
  (let (
    (template (unwrap! (map-get? proposal-templates { template-id: template-id }) ERR_TEMPLATE_NOT_FOUND))
    (voter-stake (contract-call? .UpSignal get-user-stake tx-sender))
    (current-height stacks-block-height)
    (existing-vote (map-get? template-votes { template-id: template-id, voter: tx-sender }))
  )
    ;; Validate template is active and voter hasn't voted
    (asserts! (get is-active template) ERR_TEMPLATE_INACTIVE)
    (asserts! (> voter-stake u0) ERR_INSUFFICIENT_STAKE)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    
    ;; Record the vote
    (map-set template-votes
      { template-id: template-id, voter: tx-sender }
      {
        vote: approve,
        stake: voter-stake,
        voted-at: current-height
      }
    )
    
    ;; Update template vote counts
    (map-set proposal-templates
      { template-id: template-id }
      (merge template {
        approval-votes: (if approve (+ (get approval-votes template) voter-stake) (get approval-votes template)),
        rejection-votes: (if approve (get rejection-votes template) (+ (get rejection-votes template) voter-stake))
      })
    )
    
    (ok voter-stake)
  )
)

;; Use a template (increments usage count)
(define-public (use-template (template-id uint))
  (let (
    (template (unwrap! (map-get? proposal-templates { template-id: template-id }) ERR_TEMPLATE_NOT_FOUND))
    (current-height stacks-block-height)
    (user-usage (default-to { usage-count: u0, last-used: u0 } 
                 (map-get? user-template-usage { user: tx-sender, template-id: template-id })))
  )
    ;; Validate template is active
    (asserts! (get is-active template) ERR_TEMPLATE_INACTIVE)
    
    ;; Update template usage count
    (map-set proposal-templates
      { template-id: template-id }
      (merge template { usage-count: (+ (get usage-count template) u1) })
    )
    
    ;; Update user usage tracking
    (map-set user-template-usage
      { user: tx-sender, template-id: template-id }
      {
        usage-count: (+ (get usage-count user-usage) u1),
        last-used: current-height
      }
    )
    
    (ok true)
  )
)

;; Deactivate a template (creator or owner only)
(define-public (deactivate-template (template-id uint))
  (let (
    (template (unwrap! (map-get? proposal-templates { template-id: template-id }) ERR_TEMPLATE_NOT_FOUND))
  )
    ;; Only creator or contract owner can deactivate
    (asserts! (or (is-eq tx-sender (get creator template)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    
    ;; Deactivate template
    (map-set proposal-templates
      { template-id: template-id }
      (merge template { is-active: false })
    )
    
    (ok true)
  )
)

;; Admin function to update minimum stake requirement
(define-public (update-min-template-stake (new-stake uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set min-template-stake new-stake)
    (ok new-stake)
  )
)

;; Read-only functions

;; Get template details
(define-read-only (get-template (template-id uint))
  (map-get? proposal-templates { template-id: template-id })
)

;; Get template vote by user
(define-read-only (get-template-vote (template-id uint) (voter principal))
  (map-get? template-votes { template-id: template-id, voter: voter })
)

;; Get user template usage
(define-read-only (get-user-template-usage (user principal) (template-id uint))
  (map-get? user-template-usage { user: user, template-id: template-id })
)

;; Get template counter
(define-read-only (get-template-count)
  (var-get template-counter)
)

;; Get minimum template stake
(define-read-only (get-min-template-stake)
  (var-get min-template-stake)
)

;; Calculate template approval percentage
(define-read-only (calculate-template-approval (template-id uint))
  (match (map-get? proposal-templates { template-id: template-id })
    template-data
    (let (
      (approval-votes (get approval-votes template-data))
      (rejection-votes (get rejection-votes template-data))
      (total-votes (+ approval-votes rejection-votes))
    )
      (if (> total-votes u0)
        (ok {
          approval-percentage: (/ (* approval-votes u100) total-votes),
          total-votes: total-votes,
          approval-votes: approval-votes,
          rejection-votes: rejection-votes
        })
        (ok {
          approval-percentage: u0,
          total-votes: u0,
          approval-votes: u0,
          rejection-votes: u0
        })
      )
    )
    (err ERR_TEMPLATE_NOT_FOUND)
  )
)
