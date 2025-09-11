;; title: Challenge Milestone System
;; version: 1.0.0
;; summary: Milestone-based partial payment system for innovation challenges
;; description: Enables complex challenges to be broken into milestones with incremental rewards

;; constants
(define-constant err-not-found (err u301))
(define-constant err-unauthorized (err u302))
(define-constant err-invalid-milestone (err u303))
(define-constant err-milestone-completed (err u304))
(define-constant err-invalid-reward-split (err u305))
(define-constant err-challenge-not-active (err u306))
(define-constant err-milestone-not-ready (err u307))
(define-constant err-already-approved (err u308))

(define-constant max-milestones u10)
(define-constant min-milestone-percentage u5) ;; 5% minimum per milestone

;; data vars
(define-data-var next-milestone-plan-id uint u1)

;; data maps
(define-map milestone-plans
  { plan-id: uint }
  {
    challenge-id: uint,
    creator: principal,
    total-milestones: uint,
    milestone-titles: (list 10 (string-ascii 50)),
    milestone-descriptions: (list 10 (string-utf8 200)),
    reward-percentages: (list 10 uint),
    milestone-deadlines: (list 10 uint),
    created-at: uint,
    is-active: bool
  }
)

(define-map milestone-progress
  { plan-id: uint, innovator: principal }
  {
    current-milestone: uint,
    completed-milestones: (list 10 bool),
    milestone-completions: (list 10 uint), ;; block heights when completed
    total-earned: uint,
    last-update: uint
  }
)

(define-map milestone-submissions
  { plan-id: uint, innovator: principal, milestone-index: uint }
  {
    submission-url: (string-ascii 200),
    evidence-description: (string-utf8 300),
    submitted-at: uint,
    approved: bool,
    approved-at: (optional uint),
    reviewer-feedback: (optional (string-utf8 200))
  }
)

;; public functions

(define-public (create-milestone-plan 
  (challenge-id uint) 
  (milestone-titles (list 10 (string-ascii 50)))
  (milestone-descriptions (list 10 (string-utf8 200)))
  (reward-percentages (list 10 uint))
  (milestone-deadlines (list 10 uint)))
  (let
    (
      (plan-id (var-get next-milestone-plan-id))
      (challenge (contract-call? .open-inno get-challenge challenge-id))
      (total-percentage (fold + reward-percentages u0))
      (milestone-count (len reward-percentages))
    )
    ;; Validate challenge exists and caller is creator
    (asserts! (is-some challenge) err-not-found)
    (asserts! (is-eq tx-sender (get creator (unwrap-panic challenge))) err-unauthorized)
    ;; Validate milestone parameters
    (asserts! (and (>= milestone-count u2) (<= milestone-count max-milestones)) err-invalid-milestone)
    (asserts! (is-eq total-percentage u100) err-invalid-reward-split)
    (asserts! (is-eq (len milestone-titles) milestone-count) err-invalid-milestone)
    (asserts! (is-eq (len milestone-descriptions) milestone-count) err-invalid-milestone)
    (asserts! (is-eq (len milestone-deadlines) milestone-count) err-invalid-milestone)
    
    (map-set milestone-plans
      { plan-id: plan-id }
      {
        challenge-id: challenge-id,
        creator: tx-sender,
        total-milestones: milestone-count,
        milestone-titles: milestone-titles,
        milestone-descriptions: milestone-descriptions,
        reward-percentages: reward-percentages,
        milestone-deadlines: milestone-deadlines,
        created-at: stacks-block-height,
        is-active: true
      }
    )
    
    (var-set next-milestone-plan-id (+ plan-id u1))
    (ok plan-id)
  )
)

(define-public (submit-milestone-deliverable 
  (plan-id uint) 
  (milestone-index uint) 
  (submission-url (string-ascii 200)) 
  (evidence-description (string-utf8 300)))
  (let
    (
      (plan (unwrap! (map-get? milestone-plans { plan-id: plan-id }) err-not-found))
      (progress (default-to 
                 { current-milestone: u0, completed-milestones: (list false false false false false false false false false false), 
                   milestone-completions: (list u0 u0 u0 u0 u0 u0 u0 u0 u0 u0), total-earned: u0, last-update: u0 }
                 (map-get? milestone-progress { plan-id: plan-id, innovator: tx-sender })))
      (challenge (unwrap! (contract-call? .open-inno get-challenge (get challenge-id plan)) err-not-found))
    )
    ;; Validate plan is active and milestone is next in sequence
    (asserts! (get is-active plan) err-invalid-milestone)
    (asserts! (get is-active challenge) err-challenge-not-active)
    (asserts! (is-eq milestone-index (get current-milestone progress)) err-milestone-not-ready)
    (asserts! (< milestone-index (get total-milestones plan)) err-invalid-milestone)
    
    ;; Check deadline hasn't passed
    (let ((deadline (unwrap! (element-at (get milestone-deadlines plan) milestone-index) err-invalid-milestone)))
      (asserts! (<= stacks-block-height deadline) err-invalid-milestone)
    )
    
    ;; Store milestone submission
    (map-set milestone-submissions
      { plan-id: plan-id, innovator: tx-sender, milestone-index: milestone-index }
      {
        submission-url: submission-url,
        evidence-description: evidence-description,
        submitted-at: stacks-block-height,
        approved: false,
        approved-at: none,
        reviewer-feedback: none
      }
    )
    
    (ok true)
  )
)

(define-public (approve-milestone 
  (plan-id uint) 
  (innovator principal) 
  (milestone-index uint) 
  (feedback (optional (string-utf8 200))))
  (let
    (
      (plan (unwrap! (map-get? milestone-plans { plan-id: plan-id }) err-not-found))
      (submission (unwrap! (map-get? milestone-submissions { plan-id: plan-id, innovator: innovator, milestone-index: milestone-index }) err-not-found))
      (progress (default-to 
                 { current-milestone: u0, completed-milestones: (list false false false false false false false false false false), 
                   milestone-completions: (list u0 u0 u0 u0 u0 u0 u0 u0 u0 u0), total-earned: u0, last-update: u0 }
                 (map-get? milestone-progress { plan-id: plan-id, innovator: innovator })))
      (challenge (unwrap! (contract-call? .open-inno get-challenge (get challenge-id plan)) err-not-found))
      (milestone-reward-percent (unwrap! (element-at (get reward-percentages plan) milestone-index) err-invalid-milestone))
      (milestone-reward (/ (* (get reward challenge) milestone-reward-percent) u100))
    )
    ;; Only challenge creator can approve milestones
    (asserts! (is-eq tx-sender (get creator plan)) err-unauthorized)
    (asserts! (not (get approved submission)) err-already-approved)
    
    ;; Approve milestone submission
    (map-set milestone-submissions
      { plan-id: plan-id, innovator: innovator, milestone-index: milestone-index }
      (merge submission {
        approved: true,
        approved-at: (some stacks-block-height),
        reviewer-feedback: feedback
      })
    )
    
    ;; Update progress and process payment
    (map-set milestone-progress
      { plan-id: plan-id, innovator: innovator }
      {
        current-milestone: (+ milestone-index u1),
        completed-milestones: (get completed-milestones progress), ;; Keep existing for now
        milestone-completions: (get milestone-completions progress), ;; Keep existing for now
        total-earned: (+ (get total-earned progress) milestone-reward),
        last-update: stacks-block-height
      }
    )
    
    ;; Transfer milestone reward
    (try! (as-contract (stx-transfer? milestone-reward tx-sender innovator)))
    
    (ok milestone-reward)
  )
)

(define-public (reject-milestone 
  (plan-id uint) 
  (innovator principal) 
  (milestone-index uint) 
  (feedback (string-utf8 200)))
  (let
    (
      (plan (unwrap! (map-get? milestone-plans { plan-id: plan-id }) err-not-found))
      (submission (unwrap! (map-get? milestone-submissions { plan-id: plan-id, innovator: innovator, milestone-index: milestone-index }) err-not-found))
    )
    ;; Only challenge creator can reject milestones
    (asserts! (is-eq tx-sender (get creator plan)) err-unauthorized)
    (asserts! (not (get approved submission)) err-already-approved)
    
    ;; Update submission with rejection feedback
    (map-set milestone-submissions
      { plan-id: plan-id, innovator: innovator, milestone-index: milestone-index }
      (merge submission {
        reviewer-feedback: (some feedback)
      })
    )
    
    (ok true)
  )
)

;; read-only functions

(define-read-only (get-milestone-plan (plan-id uint))
  (map-get? milestone-plans { plan-id: plan-id })
)

(define-read-only (get-milestone-progress (plan-id uint) (innovator principal))
  (map-get? milestone-progress { plan-id: plan-id, innovator: innovator })
)

(define-read-only (get-milestone-submission (plan-id uint) (innovator principal) (milestone-index uint))
  (map-get? milestone-submissions { plan-id: plan-id, innovator: innovator, milestone-index: milestone-index })
)

(define-read-only (get-next-milestone-plan-id)
  (var-get next-milestone-plan-id)
)

(define-read-only (calculate-milestone-progress-percentage (plan-id uint) (innovator principal))
  (let
    (
      (plan (map-get? milestone-plans { plan-id: plan-id }))
      (progress (map-get? milestone-progress { plan-id: plan-id, innovator: innovator }))
    )
    (match plan
      plan-data (match progress
                  prog-data (ok (/ (* (get current-milestone prog-data) u100) (get total-milestones plan-data)))
                  (ok u0))
      err-not-found)
  )
)

(define-read-only (is-milestone-deadline-passed (plan-id uint) (milestone-index uint))
  (let
    (
      (plan (map-get? milestone-plans { plan-id: plan-id }))
    )
    (match plan
      plan-data (let ((deadline (element-at (get milestone-deadlines plan-data) milestone-index)))
                  (match deadline
                    dl (ok (> stacks-block-height dl))
                    err-invalid-milestone))
      err-not-found)
  )
)

;; private helper functions

(define-private (validate-reward-percentages (percentages (list 10 uint)))
  (let ((total (fold + percentages u0)))
    (is-eq total u100))
)
