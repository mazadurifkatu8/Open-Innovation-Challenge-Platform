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
(define-constant err-voting-closed (err u109))
(define-constant err-already-voted (err u110))
(define-constant err-voting-not-started (err u111))
(define-constant err-voting-active (err u112))
(define-constant err-team-not-found (err u113))
(define-constant err-already-in-team (err u114))
(define-constant err-not-team-leader (err u115))
(define-constant err-invitation-not-found (err u116))
(define-constant err-team-full (err u117))
(define-constant err-invalid-percentage (err u118))

(define-data-var next-challenge-id uint u1)
(define-data-var next-submission-id uint u1)
(define-data-var challenge-ids (list 100 uint) (list))
(define-data-var voting-period-blocks uint u1440)
(define-data-var next-team-id uint u1)

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

(define-map challenge-voting
  { challenge-id: uint }
  {
    voting-start-block: uint,
    voting-end-block: uint,
    total-votes: uint,
    is-voting-active: bool,
    votes-calculated: bool
  }
)

(define-map submission-votes
  { challenge-id: uint, submission-id: uint }
  {
    vote-count: uint,
    weighted-votes: uint
  }
)

(define-map voter-records
  { challenge-id: uint, voter: principal }
  {
    voted-for: uint,
    vote-weight: uint,
    voted-at: uint
  }
)

;; Team collaboration data structures
(define-map teams
  { team-id: uint }
  {
    team-name: (string-ascii 50),
    leader: principal,
    challenge-id: uint,
    member-count: uint,
    max-members: uint,
    created-at: uint,
    is-active: bool
  }
)

(define-map team-members
  { team-id: uint, member: principal }
  {
    reward-percentage: uint,
    joined-at: uint,
    is-active: bool
  }
)

(define-map team-invitations
  { team-id: uint, invitee: principal }
  {
    invited-by: principal,
    invited-at: uint,
    expires-at: uint
  }
)

(define-map member-teams
  { challenge-id: uint, member: principal }
  { team-id: uint }
)

(define-map team-submissions
  { team-id: uint }
  { submission-id: uint }
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

(define-public (start-voting (challenge-id uint))
  (let
    ((challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found))
     (voting-start (+ stacks-block-height u1))
     (voting-end (+ voting-start (var-get voting-period-blocks))))
    
    (asserts! (is-eq tx-sender (get creator challenge)) err-unauthorized)
    (asserts! (not (get is-active challenge)) err-challenge-active)
    (asserts! (is-none (get winner-id challenge)) err-not-found)
    
    (map-set challenge-voting
      { challenge-id: challenge-id }
      {
        voting-start-block: voting-start,
        voting-end-block: voting-end,
        total-votes: u0,
        is-voting-active: true,
        votes-calculated: false
      }
    )
    (ok voting-end)
  )
)

(define-public (cast-vote (challenge-id uint) (submission-id uint))
  (let
    ((challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found))
     (submission (unwrap! (map-get? submissions { submission-id: submission-id }) err-not-found))
     (voting-info (unwrap! (map-get? challenge-voting { challenge-id: challenge-id }) err-voting-not-started))
     (voter-rep (map-get? innovator-reputation { innovator: tx-sender }))
     (existing-vote (map-get? voter-records { challenge-id: challenge-id, voter: tx-sender }))
     (vote-weight (+ u1 (get reputation-score (default-to 
                                              { total-submissions: u0, total-wins: u0, total-rewards-earned: u0, reputation-score: u0, last-updated: u0 }
                                              voter-rep))))
     (current-votes (default-to { vote-count: u0, weighted-votes: u0 } 
                                 (map-get? submission-votes { challenge-id: challenge-id, submission-id: submission-id }))))
    
    (asserts! (is-eq challenge-id (get challenge-id submission)) err-not-found)
    (asserts! (get is-voting-active voting-info) err-voting-closed)
    (asserts! (>= stacks-block-height (get voting-start-block voting-info)) err-voting-not-started)
    (asserts! (< stacks-block-height (get voting-end-block voting-info)) err-voting-closed)
    (asserts! (is-none existing-vote) err-already-voted)
    
    (map-set voter-records
      { challenge-id: challenge-id, voter: tx-sender }
      {
        voted-for: submission-id,
        vote-weight: vote-weight,
        voted-at: stacks-block-height
      }
    )
    
    (map-set submission-votes
      { challenge-id: challenge-id, submission-id: submission-id }
      {
        vote-count: (+ (get vote-count current-votes) u1),
        weighted-votes: (+ (get weighted-votes current-votes) vote-weight)
      }
    )
    
    (map-set challenge-voting
      { challenge-id: challenge-id }
      (merge voting-info { total-votes: (+ (get total-votes voting-info) u1) })
    )
    
    (ok vote-weight)
  )
)

(define-public (finalize-voting (challenge-id uint))
  (let
    ((challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found))
     (voting-info (unwrap! (map-get? challenge-voting { challenge-id: challenge-id }) err-voting-not-started))
     (submission-list (unwrap! (map-get? challenge-submission-list { challenge-id: challenge-id }) err-not-found)))
    
    (asserts! (is-eq tx-sender (get creator challenge)) err-unauthorized)
    (asserts! (get is-voting-active voting-info) err-voting-closed)
    (asserts! (>= stacks-block-height (get voting-end-block voting-info)) err-voting-active)
    (asserts! (not (get votes-calculated voting-info)) err-unauthorized)
    
    (let
      ((winning-submission (fold determine-winner (get submission-ids submission-list) { winner-id: u0, max-votes: u0 })))
      
      (map-set challenges
        { challenge-id: challenge-id }
        (merge challenge { winner-id: (some (get winner-id winning-submission)) })
      )
      
      (map-set challenge-voting
        { challenge-id: challenge-id }
        (merge voting-info { is-voting-active: false, votes-calculated: true })
      )
      
      (ok (get winner-id winning-submission))
    )
  )
)

(define-private (determine-winner (submission-id uint) (current-winner { winner-id: uint, max-votes: uint }))
  (let
    ((submission-vote-data (default-to { vote-count: u0, weighted-votes: u0 } 
                                        (map-get? submission-votes { challenge-id: u0, submission-id: submission-id }))))
    
    (if (> (get weighted-votes submission-vote-data) (get max-votes current-winner))
      { winner-id: submission-id, max-votes: (get weighted-votes submission-vote-data) }
      current-winner)
  )
)

(define-read-only (get-voting-info (challenge-id uint))
  (map-get? challenge-voting { challenge-id: challenge-id })
)

(define-read-only (get-submission-votes (challenge-id uint) (submission-id uint))
  (map-get? submission-votes { challenge-id: challenge-id, submission-id: submission-id })
)

(define-read-only (get-voter-record (challenge-id uint) (voter principal))
  (map-get? voter-records { challenge-id: challenge-id, voter: voter })
)

(define-read-only (is-voting-active (challenge-id uint))
  (let
    ((voting-info (map-get? challenge-voting { challenge-id: challenge-id })))
    (match voting-info
      info (and (get is-voting-active info)
                (>= stacks-block-height (get voting-start-block info))
                (< stacks-block-height (get voting-end-block info)))
      false)
  )
)

;; Team collaboration functions
(define-public (create-team (team-name (string-ascii 50)) (challenge-id uint) (max-members uint))
  (let
    ((team-id (var-get next-team-id))
     (challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) err-not-found))
     (existing-team (map-get? member-teams { challenge-id: challenge-id, member: tx-sender })))
    
    ;; Verify challenge exists and user isn't already in a team for this challenge
    (asserts! (get is-active challenge) err-challenge-closed)
    (asserts! (is-none existing-team) err-already-in-team)
    (asserts! (and (>= max-members u2) (<= max-members u10)) err-invalid-percentage)
    
    ;; Create team with leader having 50% initial allocation
    (map-set teams
      { team-id: team-id }
      {
        team-name: team-name,
        leader: tx-sender,
        challenge-id: challenge-id,
        member-count: u1,
        max-members: max-members,
        created-at: stacks-block-height,
        is-active: true
      }
    )
    
    ;; Add leader as first member
    (map-set team-members
      { team-id: team-id, member: tx-sender }
      {
        reward-percentage: u50,
        joined-at: stacks-block-height,
        is-active: true
      }
    )
    
    ;; Link member to team for this challenge
    (map-set member-teams
      { challenge-id: challenge-id, member: tx-sender }
      { team-id: team-id }
    )
    
    (var-set next-team-id (+ team-id u1))
    (ok team-id)
  )
)

(define-public (invite-to-team (team-id uint) (invitee principal))
  (let
    ((team (unwrap! (map-get? teams { team-id: team-id }) err-team-not-found))
     (invitation-expires (+ stacks-block-height u1440))) ;; 10 days to accept
    
    ;; Only team leader can invite
    (asserts! (is-eq tx-sender (get leader team)) err-not-team-leader)
    (asserts! (get is-active team) err-team-not-found)
    (asserts! (< (get member-count team) (get max-members team)) err-team-full)
    
    ;; Check if invitee already in a team for this challenge
    (asserts! (is-none (map-get? member-teams { challenge-id: (get challenge-id team), member: invitee })) err-already-in-team)
    
    ;; Create invitation
    (map-set team-invitations
      { team-id: team-id, invitee: invitee }
      {
        invited-by: tx-sender,
        invited-at: stacks-block-height,
        expires-at: invitation-expires
      }
    )
    
    (ok invitation-expires)
  )
)

(define-public (accept-team-invitation (team-id uint) (reward-percentage uint))
  (let
    ((team (unwrap! (map-get? teams { team-id: team-id }) err-team-not-found))
     (invitation (unwrap! (map-get? team-invitations { team-id: team-id, invitee: tx-sender }) err-invitation-not-found)))
    
    ;; Verify invitation is still valid
    (asserts! (< stacks-block-height (get expires-at invitation)) err-invitation-not-found)
    (asserts! (get is-active team) err-team-not-found)
    (asserts! (< (get member-count team) (get max-members team)) err-team-full)
    (asserts! (and (> reward-percentage u0) (<= reward-percentage u30)) err-invalid-percentage) ;; Max 30% per member
    
    ;; Check if user already in a team for this challenge
    (asserts! (is-none (map-get? member-teams { challenge-id: (get challenge-id team), member: tx-sender })) err-already-in-team)
    
    ;; Add member to team
    (map-set team-members
      { team-id: team-id, member: tx-sender }
      {
        reward-percentage: reward-percentage,
        joined-at: stacks-block-height,
        is-active: true
      }
    )
    
    ;; Link member to team for this challenge
    (map-set member-teams
      { challenge-id: (get challenge-id team), member: tx-sender }
      { team-id: team-id }
    )
    
    ;; Update team member count
    (map-set teams
      { team-id: team-id }
      (merge team { member-count: (+ (get member-count team) u1) })
    )
    
    ;; Remove invitation
    (map-delete team-invitations { team-id: team-id, invitee: tx-sender })
    
    (ok true)
  )
)

(define-public (submit-team-solution (team-id uint) (solution-url (string-ascii 200)) (description (string-utf8 500)))
  (let
    ((team (unwrap! (map-get? teams { team-id: team-id }) err-team-not-found))
     (submission-id (var-get next-submission-id))
     (challenge (unwrap! (map-get? challenges { challenge-id: (get challenge-id team) }) err-not-found))
     (member-data (unwrap! (map-get? team-members { team-id: team-id, member: tx-sender }) err-unauthorized)))
    
    ;; Only active team members can submit on behalf of team
    (asserts! (get is-active member-data) err-unauthorized)
    (asserts! (get is-active team) err-team-not-found)
    (asserts! (get is-active challenge) err-challenge-closed)
    
    ;; Check if team already submitted
    (asserts! (is-none (map-get? team-submissions { team-id: team-id })) err-already-submitted)
    
    ;; Create submission linked to team leader
    (map-set submissions
      { submission-id: submission-id }
      {
        challenge-id: (get challenge-id team),
        innovator: (get leader team),
        solution-url: solution-url,
        description: description,
        submitted-at: stacks-block-height
      }
    )
    
    ;; Link submission to team
    (map-set team-submissions
      { team-id: team-id }
      { submission-id: submission-id }
    )
    
    ;; Update challenge submission tracking
    (map-set challenge-submissions
      { challenge-id: (get challenge-id team), innovator: (get leader team) }
      { submission-id: submission-id }
    )
    
    (map-set challenge-submission-list
      { challenge-id: (get challenge-id team) }
      { submission-ids: (unwrap! (as-max-len? 
                                   (append (get submission-ids (default-to { submission-ids: (list) } 
                                                               (map-get? challenge-submission-list { challenge-id: (get challenge-id team) }))) 
                                           submission-id) 
                                   u100) 
                                 err-unauthorized) }
    )
    
    (var-set next-submission-id (+ submission-id u1))
    (ok submission-id)
  )
)

(define-public (distribute-team-reward (team-id uint))
  (let
    ((team (unwrap! (map-get? teams { team-id: team-id }) err-team-not-found))
     (challenge (unwrap! (map-get? challenges { challenge-id: (get challenge-id team) }) err-not-found))
     (team-submission (unwrap! (map-get? team-submissions { team-id: team-id }) err-not-found))
     (submission (unwrap! (map-get? submissions { submission-id: (get submission-id team-submission) }) err-not-found)))
    
    ;; Verify this team won and reward hasn't been distributed
    (asserts! (is-some (get winner-id challenge)) err-not-winner)
    (asserts! (is-eq (unwrap-panic (get winner-id challenge)) (get submission-id team-submission)) err-not-winner)
    (asserts! (not (get is-paid challenge)) err-already-paid)
    
    ;; Only team leader can trigger distribution
    (asserts! (is-eq tx-sender (get leader team)) err-not-team-leader)
    
    ;; Mark challenge as paid to prevent double distribution
    (map-set challenges
      { challenge-id: (get challenge-id team) }
      (merge challenge { is-paid: true })
    )
    
    ;; Distribute rewards to team members based on percentages
    (try! (distribute-to-members team-id (get reward challenge)))
    
    (ok true)
  )
)

;; Helper function to distribute rewards to all team members
(define-private (distribute-to-members (team-id uint) (total-reward uint))
  (let
    ((team (unwrap-panic (map-get? teams { team-id: team-id }))))
    ;; In a real implementation, this would iterate through all team members
    ;; For now, we'll handle the leader's portion
    (let
      ((leader-data (unwrap-panic (map-get? team-members { team-id: team-id, member: (get leader team) })))
       (leader-amount (/ (* total-reward (get reward-percentage leader-data)) u100)))
      
      ;; Transfer leader's portion
      (as-contract (stx-transfer? leader-amount tx-sender (get leader team)))
    )
  )
)

;; Read-only functions for team management
(define-read-only (get-team (team-id uint))
  (map-get? teams { team-id: team-id })
)

(define-read-only (get-team-member (team-id uint) (member principal))
  (map-get? team-members { team-id: team-id, member: member })
)

(define-read-only (get-team-invitation (team-id uint) (invitee principal))
  (map-get? team-invitations { team-id: team-id, invitee: invitee })
)

(define-read-only (get-member-team (challenge-id uint) (member principal))
  (map-get? member-teams { challenge-id: challenge-id, member: member })
)

(define-read-only (get-team-submission (team-id uint))
  (map-get? team-submissions { team-id: team-id })
)

(define-public (initialize-system)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (initialize-reputation-tiers)
    (ok true)
  )
)






