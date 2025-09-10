;; Skill Verification System
;; Peer-to-peer skill validation for time banking ecosystem

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_SKILL_NOT_FOUND (err u201))
(define-constant ERR_VERIFICATION_NOT_FOUND (err u202))
(define-constant ERR_ALREADY_VERIFIED (err u203))
(define-constant ERR_CANNOT_VERIFY_SELF (err u204))
(define-constant ERR_INSUFFICIENT_INTERACTIONS (err u205))
(define-constant ERR_INVALID_SKILL_LEVEL (err u206))

;; Data variables
(define-data-var skill-counter uint u0)
(define-data-var verification-counter uint u0)
(define-data-var min-interactions-required uint u2)

;; User skills and their claimed proficiency levels
(define-map user-skills { user: principal, skill-id: uint } {
  skill-name: (string-ascii 50),
  proficiency-level: uint, ;; 1-5 scale
  years-experience: uint,
  description: (string-ascii 200),
  verified: bool,
  verification-count: uint,
  claimed-at: uint
})

;; Skill verification requests and responses
(define-map skill-verifications { verification-id: uint } {
  skill-owner: principal,
  verifier: principal,
  skill-id: uint,
  verified-level: uint,
  verification-notes: (string-ascii 300),
  interaction-count: uint,
  status: (string-ascii 20),
  created-at: uint,
  verified-at: uint
})

;; Track interactions between users for verification eligibility
(define-map user-interactions { user-a: principal, user-b: principal } {
  interaction-count: uint,
  last-interaction: uint,
  total-hours-worked: uint
})

;; Skill categories for organization
(define-map skill-categories { category-id: uint } {
  name: (string-ascii 50),
  description: (string-ascii 200),
  active: bool
})

;; Claim a skill with proficiency level
(define-public (claim-skill (skill-name (string-ascii 50)) (proficiency-level uint) (years-experience uint) (description (string-ascii 200)))
  (let (
    (skill-id (+ (var-get skill-counter) u1))
    (user tx-sender)
  )
    (asserts! (and (>= proficiency-level u1) (<= proficiency-level u5)) ERR_INVALID_SKILL_LEVEL)
    (map-set user-skills { user: user, skill-id: skill-id } {
      skill-name: skill-name,
      proficiency-level: proficiency-level,
      years-experience: years-experience,
      description: description,
      verified: false,
      verification-count: u0,
      claimed-at: stacks-block-height
    })
    (var-set skill-counter skill-id)
    (ok skill-id)
  )
)

;; Request verification for a claimed skill
(define-public (request-skill-verification (skill-owner principal) (skill-id uint) (verifier principal))
  (let (
    (skill (map-get? user-skills { user: skill-owner, skill-id: skill-id }))
    (interactions (get-user-interaction-data skill-owner verifier))
    (verification-id (+ (var-get verification-counter) u1))
  )
    (asserts! (is-some skill) ERR_SKILL_NOT_FOUND)
    (asserts! (not (is-eq skill-owner verifier)) ERR_CANNOT_VERIFY_SELF)
    (asserts! (>= (get interaction-count interactions) (var-get min-interactions-required)) ERR_INSUFFICIENT_INTERACTIONS)
    
    (map-set skill-verifications { verification-id: verification-id } {
      skill-owner: skill-owner,
      verifier: verifier,
      skill-id: skill-id,
      verified-level: u0,
      verification-notes: "",
      interaction-count: (get interaction-count interactions),
      status: "pending",
      created-at: stacks-block-height,
      verified-at: u0
    })
    (var-set verification-counter verification-id)
    (ok verification-id)
  )
)

;; Verify a skill (by the requested verifier)
(define-public (verify-skill (verification-id uint) (verified-level uint) (notes (string-ascii 300)))
  (let (
    (verification (map-get? skill-verifications { verification-id: verification-id }))
  )
    (asserts! (is-some verification) ERR_VERIFICATION_NOT_FOUND)
    (asserts! (and (>= verified-level u1) (<= verified-level u5)) ERR_INVALID_SKILL_LEVEL)
    
    (let (
      (verification-data (unwrap-panic verification))
      (skill-owner (get skill-owner verification-data))
      (skill-id (get skill-id verification-data))
      (verifier (get verifier verification-data))
    )
      (asserts! (is-eq tx-sender verifier) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status verification-data) "pending") ERR_ALREADY_VERIFIED)
      
      (let (
        (skill (unwrap-panic (map-get? user-skills { user: skill-owner, skill-id: skill-id })))
        (new-verification-count (+ (get verification-count skill) u1))
        (is-verified (>= new-verification-count u3))
      )
        ;; Update verification record
        (map-set skill-verifications { verification-id: verification-id } 
          (merge verification-data {
            verified-level: verified-level,
            verification-notes: notes,
            status: "completed",
            verified-at: stacks-block-height
          })
        )
        
        ;; Update skill record
        (map-set user-skills { user: skill-owner, skill-id: skill-id }
          (merge skill {
            verification-count: new-verification-count,
            verified: is-verified
          })
        )
        
        (ok true)
      )
    )
  )
)

;; Record user interaction (called by main banking contract)
(define-public (record-user-interaction (user-a principal) (user-b principal) (hours-worked uint))
  (let (
    (current-interactions (get-user-interaction-data user-a user-b))
    (new-count (+ (get interaction-count current-interactions) u1))
    (new-hours (+ (get total-hours-worked current-interactions) hours-worked))
  )
    (map-set user-interactions { user-a: user-a, user-b: user-b } {
      interaction-count: new-count,
      last-interaction: stacks-block-height,
      total-hours-worked: new-hours
    })
    
    ;; Also record reverse interaction
    (let (
      (reverse-interactions (get-user-interaction-data user-b user-a))
      (reverse-count (+ (get interaction-count reverse-interactions) u1))
      (reverse-hours (+ (get total-hours-worked reverse-interactions) hours-worked))
    )
      (map-set user-interactions { user-a: user-b, user-b: user-a } {
        interaction-count: reverse-count,
        last-interaction: stacks-block-height,
        total-hours-worked: reverse-hours
      })
    )
    (ok true)
  )
)

;; Update minimum interaction requirement (admin only)
(define-public (update-min-interactions (new-minimum uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set min-interactions-required new-minimum)
    (ok true)
  )
)

;; Get user's verified skills count
(define-read-only (get-user-verified-skills-count (user principal))
  (let (
    (skill-1 (map-get? user-skills { user: user, skill-id: u1 }))
    (skill-2 (map-get? user-skills { user: user, skill-id: u2 }))
    (skill-3 (map-get? user-skills { user: user, skill-id: u3 }))
    (skill-4 (map-get? user-skills { user: user, skill-id: u4 }))
    (skill-5 (map-get? user-skills { user: user, skill-id: u5 }))
  )
    (+ (if (and (is-some skill-1) (get verified (unwrap-panic skill-1))) u1 u0)
       (+ (if (and (is-some skill-2) (get verified (unwrap-panic skill-2))) u1 u0)
          (+ (if (and (is-some skill-3) (get verified (unwrap-panic skill-3))) u1 u0)
             (+ (if (and (is-some skill-4) (get verified (unwrap-panic skill-4))) u1 u0)
                (if (and (is-some skill-5) (get verified (unwrap-panic skill-5))) u1 u0)
             )
          )
       )
    )
  )
)

;; Read-only functions
(define-read-only (get-user-skill (user principal) (skill-id uint))
  (map-get? user-skills { user: user, skill-id: skill-id })
)

(define-read-only (get-skill-verification (verification-id uint))
  (map-get? skill-verifications { verification-id: verification-id })
)

(define-read-only (get-user-interactions (user-a principal) (user-b principal))
  (map-get? user-interactions { user-a: user-a, user-b: user-b })
)

(define-read-only (get-skill-counter)
  (var-get skill-counter)
)

(define-read-only (get-verification-counter)
  (var-get verification-counter)
)

(define-read-only (get-min-interactions-required)
  (var-get min-interactions-required)
)

(define-read-only (can-verify-skill (verifier principal) (skill-owner principal))
  (let (
    (interactions (get-user-interaction-data skill-owner verifier))
  )
    (and 
      (not (is-eq verifier skill-owner))
      (>= (get interaction-count interactions) (var-get min-interactions-required))
    )
  )
)

;; Helper functions
(define-private (get-user-interaction-data (user-a principal) (user-b principal))
  (default-to
    { interaction-count: u0, last-interaction: u0, total-hours-worked: u0 }
    (map-get? user-interactions { user-a: user-a, user-b: user-b })
  )
)
