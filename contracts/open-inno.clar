(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-challenge-closed (err u102))
(define-constant err-already-submitted (err u103))
(define-constant err-not-winner (err u104))
(define-constant err-already-paid (err u105))
(define-constant err-insufficient-funds (err u106))
(define-constant err-unauthorized (err u107))
(define-constant err-challenge-active (err u108))

(define-data-var next-challenge-id uint u1)
(define-data-var next-submission-id uint u1)
(define-data-var challenge-ids (list 100 uint) (list))

(define-map challenges
  { challenge-id: uint }
  {
    title: (string-ascii 100),
    description: (string-utf8 500),
    reward: uint,
    creator: principal,
    is-active: bool,
    winner-id: (optional uint),
    is-paid: bool,
    created-at: uint
  }
)

(define-map submissions
  { submission-id: uint }
  {
    challenge-id: uint,
    innovator: principal,
    solution-url: (string-ascii 200),
    description: (string-utf8 500),
    submitted-at: uint
  }
)

(define-map challenge-submissions
  { challenge-id: uint, innovator: principal }
  { submission-id: uint }
)

(define-map challenge-submission-list
  { challenge-id: uint }
  { submission-ids: (list 100 uint) }
)

(define-public (create-challenge (title (string-ascii 100)) (description (string-utf8 500)) (reward uint))
  (let
    ((challenge-id (var-get next-challenge-id)))
    (asserts! (>= reward u0) err-insufficient-funds)
    (try! (stx-transfer? reward tx-sender (as-contract tx-sender)))
    (map-set challenges
      { challenge-id: challenge-id }
      {
        title: title,
        description: description,
        reward: reward,
        creator: tx-sender,
        is-active: true,
        winner-id: none,
        is-paid: false,
        created-at: stacks-block-height
      }
    )
    (map-set challenge-submission-list
      { challenge-id: challenge-id }
      { submission-ids: (list) }
    )
    (var-set next-challenge-id (+ challenge-id u1))
    (ok challenge-id)
  )
)

(define-public (submit-solution (challenge-id uint) (solution-url (string-ascii 200)) (description (string-utf8 500)))
  (let
    ((submission-id (var-get next-submission-id))
     (challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found))
     (existing-submission (map-get? challenge-submissions { challenge-id: challenge-id, innovator: tx-sender })))
    
    (asserts! (get is-active challenge) err-challenge-closed)
    (asserts! (is-none existing-submission) err-already-submitted)
    
    (map-set submissions
      { submission-id: submission-id }
      {
        challenge-id: challenge-id,
        innovator: tx-sender,
        solution-url: solution-url,
        description: description,
        submitted-at: stacks-block-height
      }
    )
    
    (map-set challenge-submissions
      { challenge-id: challenge-id, innovator: tx-sender }
      { submission-id: submission-id }
    )
    
    (map-set challenge-submission-list
      { challenge-id: challenge-id }
      { submission-ids: (unwrap! (as-max-len? 
                                   (append (get submission-ids (default-to { submission-ids: (list) } 
                                                               (map-get? challenge-submission-list { challenge-id: challenge-id }))) 
                                           submission-id) 
                                   u100) 
                                 err-unauthorized) }
    )
    
    (var-set next-submission-id (+ submission-id u1))
    (ok submission-id)
  )
)

(define-public (select-winner (challenge-id uint) (submission-id uint))
  (let
    ((challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found))
     (submission (unwrap! (map-get? submissions { submission-id: submission-id }) err-not-found)))
    
    (asserts! (is-eq tx-sender (get creator challenge)) err-unauthorized)
    (asserts! (is-eq challenge-id (get challenge-id submission)) err-not-found)
    (asserts! (get is-active challenge) err-challenge-closed)
    
    (map-set challenges
      { challenge-id: challenge-id }
      (merge challenge { is-active: false, winner-id: (some submission-id) })
    )
    
    (ok submission-id)
  )
)

(define-public (claim-reward (challenge-id uint))
  (let
    ((challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found))
     (winner-id (unwrap! (get winner-id challenge) err-not-winner))
     (submission (unwrap! (map-get? submissions { submission-id: winner-id }) err-not-found)))
    
    (asserts! (is-eq tx-sender (get innovator submission)) err-unauthorized)
    (asserts! (not (get is-active challenge)) err-challenge-active)
    (asserts! (not (get is-paid challenge)) err-already-paid)
    
    (map-set challenges
      { challenge-id: challenge-id }
      (merge challenge { is-paid: true })
    )
    
    (as-contract (stx-transfer? (get reward challenge) tx-sender tx-sender))
  )
)

(define-public (close-challenge (challenge-id uint))
  (let
    ((challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found)))
    
    (asserts! (is-eq tx-sender (get creator challenge)) err-unauthorized)
    (asserts! (get is-active challenge) err-challenge-closed)
    
    (map-set challenges
      { challenge-id: challenge-id }
      (merge challenge { is-active: false })
    )
    
    (as-contract (stx-transfer? (get reward challenge) tx-sender (get creator challenge)))
  )
)

(define-read-only (get-challenge (challenge-id uint))
  (map-get? challenges { challenge-id: challenge-id })
)

(define-read-only (get-submission (submission-id uint))
  (map-get? submissions { submission-id: submission-id })
)

(define-read-only (get-challenge-submissions (challenge-id uint))
  (map-get? challenge-submission-list { challenge-id: challenge-id })
)

(define-read-only (get-user-submission (challenge-id uint) (innovator principal))
  (map-get? challenge-submissions { challenge-id: challenge-id, innovator: innovator })
)

(define-read-only (get-active-challenges)
  (ok true)
)



(define-constant categories (list 
    "defi"
    "nft" 
    "gaming"
    "infrastructure"
    "social"
))

(define-map challenge-categories
    { challenge-id: uint }
    { category: (string-ascii 20) }
)

(define-public (set-challenge-category (challenge-id uint) (category (string-ascii 14)))
    (let (
        (challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found))
    )
        (asserts! (is-eq tx-sender (get creator challenge)) err-unauthorized)
        (asserts! (is-some (index-of categories category)) err-unauthorized)
        (ok (map-set challenge-categories { challenge-id: challenge-id } { category: category }))
    )
)

(define-private (check-category (challenge-id uint) (category (string-ascii 14)))
    (let ((challenge-category (map-get? challenge-categories { challenge-id: challenge-id })))
        (and 
            (is-some challenge-category)
            (is-eq (get category (default-to { category: "" } challenge-category)) category)
        )
    )
)

(define-private (check-category-helper (val uint))
    val
)

;; (define-read-only (get-challenges-by-category (category (string-ascii 20)))
;;     (ok (filter check-category (map check-category-helper (var-get challenge-ids))))
;; )


(define-map challenge-milestones 
    { challenge-id: uint }
    { 
        total-milestones: uint,
        completed-milestones: uint,
        milestone-rewards: (list 5 uint)
    }
)

(define-public (create-challenge-milestones (challenge-id uint) (milestone-rewards (list 5 uint)))
    (let (
        (challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found))
        (total-reward (fold + milestone-rewards u0))
    )
        (asserts! (is-eq tx-sender (get creator challenge)) err-unauthorized)
        (asserts! (is-eq total-reward (get reward challenge)) err-insufficient-funds)
        (ok (map-set challenge-milestones 
            { challenge-id: challenge-id }
            {
                total-milestones: (len milestone-rewards),
                completed-milestones: u0,
                milestone-rewards: milestone-rewards
            }
        ))
    )
)

(define-public (complete-milestone (challenge-id uint))
    (let (
        (challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found))
        (milestones (unwrap! (map-get? challenge-milestones { challenge-id: challenge-id }) err-not-found))
        (current-milestone (get completed-milestones milestones))
        (milestone-reward (unwrap! (element-at (get milestone-rewards milestones) current-milestone) err-not-found))
    )
        (asserts! (is-eq tx-sender (get creator challenge)) err-unauthorized)
        (asserts! (< current-milestone (get total-milestones milestones)) err-unauthorized)
        (try! (as-contract (stx-transfer? milestone-reward tx-sender (get innovator (unwrap! (map-get? submissions { submission-id: (unwrap! (get winner-id challenge) err-not-winner) }) err-not-found)))))
        (ok (map-set challenge-milestones 
            { challenge-id: challenge-id }
            (merge milestones { completed-milestones: (+ current-milestone u1) })
        ))
    )
)

(define-map innovator-reputation
  { innovator: principal }
  {
    total-submissions: uint,
    total-wins: uint,
    total-rewards-earned: uint,
    reputation-score: uint,
    last-updated: uint
  }
)

(define-map reputation-tiers
  { tier-level: uint }
  {
    tier-name: (string-ascii 20),
    min-score: uint,
    max-score: uint,
    benefits: (string-ascii 100)
  }
)

(define-data-var reputation-multiplier uint u100)

(define-private (initialize-reputation-tiers)
  (begin
    (map-set reputation-tiers { tier-level: u1 } 
      { tier-name: "Bronze", min-score: u0, max-score: u299, benefits: "Basic access" })
    (map-set reputation-tiers { tier-level: u2 } 
      { tier-name: "Silver", min-score: u300, max-score: u699, benefits: "Priority support" })
    (map-set reputation-tiers { tier-level: u3 } 
      { tier-name: "Gold", min-score: u700, max-score: u1199, benefits: "Exclusive challenges" })
    (map-set reputation-tiers { tier-level: u4 } 
      { tier-name: "Platinum", min-score: u1200, max-score: u999999, benefits: "All benefits" })
  )
)

(define-private (calculate-reputation-score (submission-count uint) (wins uint) (rewards uint))
  (let
    ((win-rate (if (> submission-count u0) (/ (* wins u100) submission-count) u0))
     (base-score (+ (* wins u50) (* submission-count u10)))
     (bonus-score (/ rewards u1000000)))
    (+ base-score bonus-score (* win-rate u2))
  )
)

(define-private (update-innovator-reputation (innovator principal) (is-win bool) (reward uint))
  (let
    ((current-rep (default-to 
                    { total-submissions: u0, total-wins: u0, total-rewards-earned: u0, reputation-score: u0, last-updated: u0 }
                    (map-get? innovator-reputation { innovator: innovator })))
     (new-submissions (+ (get total-submissions current-rep) u1))
     (new-wins (if is-win (+ (get total-wins current-rep) u1) (get total-wins current-rep)))
     (new-rewards (+ (get total-rewards-earned current-rep) reward))
     (new-score (calculate-reputation-score new-submissions new-wins new-rewards)))
    
    (map-set innovator-reputation
      { innovator: innovator }
      {
        total-submissions: new-submissions,
        total-wins: new-wins,
        total-rewards-earned: new-rewards,
        reputation-score: new-score,
        last-updated: stacks-block-height
      }
    )
  )
)

(define-private (get-innovator-tier (reputation-score uint))
  (if (<= reputation-score u299) u1
    (if (<= reputation-score u699) u2
      (if (<= reputation-score u1199) u3 u4)))
)

(define-public (submit-solution-with-reputation (challenge-id uint) (solution-url (string-ascii 200)) (description (string-utf8 500)))
  (let
    ((submission-result (try! (submit-solution challenge-id solution-url description))))
    (update-innovator-reputation tx-sender false u0)
    (ok submission-result)
  )
)

(define-public (select-winner-with-reputation (challenge-id uint) (submission-id uint))
  (let
    ((challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found))
     (submission (unwrap! (map-get? submissions { submission-id: submission-id }) err-not-found))
     (winner-result (try! (select-winner challenge-id submission-id))))
    
    (update-innovator-reputation (get innovator submission) true (get reward challenge))
    (ok winner-result)
  )
)

(define-public (create-reputation-gated-challenge 
  (title (string-ascii 100)) 
  (description (string-utf8 500)) 
  (reward uint) 
  (min-tier uint))
  (let
    ((challenge-id (try! (create-challenge title description reward))))
    (asserts! (<= min-tier u4) err-unauthorized)
    (map-set challenge-reputation-requirements
      { challenge-id: challenge-id }
      { min-tier: min-tier }
    )
    (ok challenge-id)
  )
)

(define-map challenge-reputation-requirements
  { challenge-id: uint }
  { min-tier: uint }
)

(define-public (submit-to-gated-challenge (challenge-id uint) (solution-url (string-ascii 200)) (description (string-utf8 500)))
  (let
    ((challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found))
     (requirements (map-get? challenge-reputation-requirements { challenge-id: challenge-id }))
     (innovator-rep (map-get? innovator-reputation { innovator: tx-sender })))
    
    (match requirements
      req-data (let
                 ((required-tier (get min-tier req-data))
                  (innovator-score (get reputation-score (default-to 
                                                          { total-submissions: u0, total-wins: u0, total-rewards-earned: u0, reputation-score: u0, last-updated: u0 }
                                                          innovator-rep)))
                  (innovator-tier (get-innovator-tier innovator-score)))
                 (asserts! (>= innovator-tier required-tier) err-unauthorized)
                 (submit-solution-with-reputation challenge-id solution-url description))
      (submit-solution-with-reputation challenge-id solution-url description))
  )
)

(define-read-only (get-innovator-reputation (innovator principal))
  (map-get? innovator-reputation { innovator: innovator })
)

(define-read-only (get-innovator-tier-info (innovator principal))
  (let
    ((rep-data (map-get? innovator-reputation { innovator: innovator })))
    (match rep-data
      data (let
             ((tier-level (get-innovator-tier (get reputation-score data))))
             (map-get? reputation-tiers { tier-level: tier-level }))
      none)
  )
)

(define-read-only (get-reputation-tier (tier-level uint))
  (map-get? reputation-tiers { tier-level: tier-level })
)

(define-read-only (get-challenge-requirements (challenge-id uint))
  (map-get? challenge-reputation-requirements { challenge-id: challenge-id })
)

(define-public (initialize-system)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (initialize-reputation-tiers)
    (ok true)
  )
)