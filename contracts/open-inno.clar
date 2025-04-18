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