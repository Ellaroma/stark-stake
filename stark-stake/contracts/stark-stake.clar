;; StarkStake - Revolutionary Quadratic Voting DAO Governance Platform

;; Error Constants
(define-constant ERR-UNAUTHORIZED (err u1001))
(define-constant ERR-INVALID-AMOUNT (err u1002))
(define-constant ERR-INVALID-DURATION (err u1003))
(define-constant ERR-STAKE-NOT-FOUND (err u1004))
(define-constant ERR-INSUFFICIENT-STAKE (err u1005))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u1006))
(define-constant ERR-VOTING-CLOSED (err u1007))
(define-constant ERR-ALREADY-VOTED (err u1008))
(define-constant ERR-INVALID-PROPOSAL (err u1009))
(define-constant ERR-QUORUM-NOT-MET (err u1010))
(define-constant ERR-INVALID-TIMELOCK (err u1011))
(define-constant ERR-DELEGATION-FAILED (err u1012))
(define-constant ERR-TREASURY-INSUFFICIENT (err u1013))
(define-constant ERR-INVALID-CATEGORY (err u1014))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-STAKE-DURATION u365) ;; 365 days
(define-constant MIN-STAKE-DURATION u7)   ;; 7 days
(define-constant BASE-VOTING-WEIGHT u100)
(define-constant QUADRATIC-MULTIPLIER u150)
(define-constant CONVICTION-THRESHOLD u1000)

;; Data Variables
(define-data-var next-proposal-id uint u1)
(define-data-var treasury-balance uint u0)
(define-data-var base-quorum uint u10) ;; 10%
(define-data-var governance-token principal .governance-token)
(define-data-var protocol-active bool true)
(define-data-var total-staked uint u0)
(define-data-var conviction-decay-rate uint u95) ;; 95% retention per period

;; Data Maps
(define-map stakes 
  { user: principal } 
  { 
    amount: uint, 
    duration: uint, 
    start-block: uint, 
    conviction-score: uint,
    participation-count: uint,
    last-activity: uint
  })

(define-map proposals 
  { id: uint } 
  { 
    creator: principal, 
    title: (string-ascii 100), 
    description: (string-ascii 500),
    category: uint, ;; 1=treasury, 2=governance, 3=protocol
    votes-for: uint, 
    votes-against: uint, 
    start-block: uint, 
    end-block: uint,
    executed: bool,
    quorum-required: uint,
    treasury-amount: uint
  })

(define-map user-votes 
  { proposal-id: uint, user: principal } 
  { 
    weight: uint, 
    support: bool, 
    timestamp: uint 
  })

(define-map delegation-registry 
  { delegator: principal } 
  { 
    delegate: principal, 
    delegated-weight: uint, 
    active: bool 
  })

(define-map conviction-history 
  { user: principal, period: uint } 
  { 
    score: uint, 
    participation: uint, 
    consistency: uint 
  })

(define-map dao-configurations 
  { dao-id: principal } 
  { 
    voting-period: uint, 
    execution-delay: uint, 
    custom-quorum: uint, 
    features-enabled: uint 
  })

(define-map treasury-allocations 
  { proposal-id: uint } 
  { 
    recipient: principal, 
    amount: uint, 
    category: uint, 
    executed: bool 
  })

(define-map reputation-scores 
  { user: principal } 
  { 
    base-score: uint, 
    community-rating: uint, 
    proposal-success-rate: uint, 
    governance-activity: uint 
  })

;; Helper Functions
(define-private (calculate-quadratic-voting-weight (amount uint) (conviction uint))
  (let (
    (base-weight (/ (* amount BASE-VOTING-WEIGHT) u1000000)) ;; Normalize amount
    (conviction-bonus (/ (* conviction QUADRATIC-MULTIPLIER) u10000))
    (total-weight (+ base-weight conviction-bonus))
  )
    ;; Apply quadratic formula: sqrt(weight) * multiplier
    (/ (* (sqrti total-weight) u100) u10)))

(define-private (calculate-dynamic-quorum (category uint))
  (let (
    (base (var-get base-quorum))
  )
    (if (is-eq category u1) ;; Treasury proposals need higher quorum
      (+ base u10)
      (if (is-eq category u2) ;; Governance proposals
        (+ base u5)
        base)))) ;; Protocol proposals use base quorum

(define-private (update-conviction-score (user principal))
  (let (
    (current-stake (unwrap! (map-get? stakes { user: user }) ERR-STAKE-NOT-FOUND))
    (blocks-passed (- block-height (get last-activity current-stake)))
    (decay-factor (if (> blocks-passed u1440) ;; ~10 days
      (var-get conviction-decay-rate)
      u100))
    (new-conviction (/ (* (get conviction-score current-stake) decay-factor) u100))
  )
    (ok new-conviction)))

;; Admin Functions
(define-public (set-governance-token (new-token principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set governance-token new-token)
    (ok true)))

(define-public (update-protocol-status (active bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set protocol-active active)
    (ok true)))

(define-public (adjust-base-quorum (new-quorum uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (<= new-quorum u50) ERR-INVALID-AMOUNT) ;; Max 50%
    (var-set base-quorum new-quorum)
    (ok true)))

(define-public (fund-treasury (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (ok true)))

;; Core Staking Functions
(define-public (stake-tokens (amount uint) (duration uint))
  (let (
    (current-block block-height)
    (existing-stake (default-to 
      { amount: u0, duration: u0, start-block: u0, conviction-score: u0, participation-count: u0, last-activity: u0 }
      (map-get? stakes { user: tx-sender })))
  )
    (asserts! (var-get protocol-active) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (and (>= duration MIN-STAKE-DURATION) (<= duration MAX-STAKE-DURATION)) ERR-INVALID-DURATION)
    
    ;; Calculate conviction score based on amount, duration, and history
    (let (
      (base-conviction (/ (* amount duration) u100))
      (time-multiplier (if (> duration u30) u120 u100))
      (new-conviction (/ (* base-conviction time-multiplier) u100))
      (updated-stake {
        amount: (+ (get amount existing-stake) amount),
        duration: duration,
        start-block: current-block,
        conviction-score: (+ (get conviction-score existing-stake) new-conviction),
        participation-count: (get participation-count existing-stake),
        last-activity: current-block
      })
    )
      (map-set stakes { user: tx-sender } updated-stake)
      (var-set total-staked (+ (var-get total-staked) amount))
      (ok new-conviction))))

(define-public (unstake-tokens (amount uint))
  (let (
    (user-stake (unwrap! (map-get? stakes { user: tx-sender }) ERR-STAKE-NOT-FOUND))
    (stake-end (+ (get start-block user-stake) (get duration user-stake)))
  )
    (asserts! (>= block-height stake-end) ERR-INVALID-DURATION)
    (asserts! (>= (get amount user-stake) amount) ERR-INSUFFICIENT-STAKE)
    
    (let (
      (updated-stake (merge user-stake { 
        amount: (- (get amount user-stake) amount),
        conviction-score: (/ (* (get conviction-score user-stake) (- (get amount user-stake) amount)) (get amount user-stake))
      }))
    )
      (map-set stakes { user: tx-sender } updated-stake)
      (var-set total-staked (- (var-get total-staked) amount))
      (ok amount))))

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (category uint) (treasury-amount uint))
  (let (
    (proposal-id (var-get next-proposal-id))
    (user-stake (unwrap! (map-get? stakes { user: tx-sender }) ERR-STAKE-NOT-FOUND))
    (voting-period u1440) ;; ~10 days in blocks
    (dynamic-quorum (calculate-dynamic-quorum category))
  )
    (asserts! (var-get protocol-active) ERR-UNAUTHORIZED)
    (asserts! (> (get conviction-score user-stake) CONVICTION-THRESHOLD) ERR-INSUFFICIENT-STAKE)
    (asserts! (and (>= category u1) (<= category u3)) ERR-INVALID-CATEGORY)
    (asserts! (or (is-eq treasury-amount u0) (<= treasury-amount (var-get treasury-balance))) ERR-TREASURY-INSUFFICIENT)
    
    (map-set proposals 
      { id: proposal-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        category: category,
        votes-for: u0,
        votes-against: u0,
        start-block: block-height,
        end-block: (+ block-height voting-period),
        executed: false,
        quorum-required: dynamic-quorum,
        treasury-amount: treasury-amount
      })
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)))

(define-public (vote-on-proposal (proposal-id uint) (support bool))
  (let (
    (proposal (unwrap! (map-get? proposals { id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (user-stake (unwrap! (map-get? stakes { user: tx-sender }) ERR-STAKE-NOT-FOUND))
    (existing-vote (map-get? user-votes { proposal-id: proposal-id, user: tx-sender }))
    (voting-weight (calculate-quadratic-voting-weight (get amount user-stake) (get conviction-score user-stake)))
  )
    (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
    (asserts! (<= block-height (get end-block proposal)) ERR-VOTING-CLOSED)
    (asserts! (not (get executed proposal)) ERR-VOTING-CLOSED)
    
    ;; Record vote
    (map-set user-votes 
      { proposal-id: proposal-id, user: tx-sender }
      { weight: voting-weight, support: support, timestamp: block-height })
    
    ;; Update proposal vote counts
    (let (
      (updated-proposal (merge proposal {
        votes-for: (if support (+ (get votes-for proposal) voting-weight) (get votes-for proposal)),
        votes-against: (if support (get votes-against proposal) (+ (get votes-against proposal) voting-weight))
      }))
    )
      (map-set proposals { id: proposal-id } updated-proposal)
      
      ;; Update user participation
      (map-set stakes 
        { user: tx-sender } 
        (merge user-stake { 
          participation-count: (+ (get participation-count user-stake) u1),
          last-activity: block-height 
        }))
      
      (ok voting-weight))))

(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
    (total-supply (var-get total-staked))
    (quorum-met (>= (* total-votes u100) (* total-supply (get quorum-required proposal))))
  )
    (asserts! (> block-height (get end-block proposal)) ERR-VOTING-CLOSED)
    (asserts! (not (get executed proposal)) ERR-ALREADY-VOTED)
    (asserts! quorum-met ERR-QUORUM-NOT-MET)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR-INVALID-PROPOSAL)
    
    ;; Mark as executed
    (map-set proposals { id: proposal-id } (merge proposal { executed: true }))
    
    ;; Handle treasury allocation if needed
    (if (> (get treasury-amount proposal) u0)
      (begin
        (map-set treasury-allocations 
          { proposal-id: proposal-id }
          { 
            recipient: (get creator proposal), 
            amount: (get treasury-amount proposal), 
            category: (get category proposal), 
            executed: true 
          })
        (var-set treasury-balance (- (var-get treasury-balance) (get treasury-amount proposal))))
      true)
    
    (ok true)))

(define-public (delegate-voting-power (delegate principal))
  (let (
    (user-stake (unwrap! (map-get? stakes { user: tx-sender }) ERR-STAKE-NOT-FOUND))
    (delegated-weight (get conviction-score user-stake))
  )
    (asserts! (not (is-eq tx-sender delegate)) ERR-DELEGATION-FAILED)
    (asserts! (> delegated-weight u0) ERR-INSUFFICIENT-STAKE)
    
    (map-set delegation-registry 
      { delegator: tx-sender }
      { 
        delegate: delegate, 
        delegated-weight: delegated-weight, 
        active: true 
      })
    
    (ok delegated-weight)))

(define-public (revoke-delegation)
  (begin
    (asserts! (is-some (map-get? delegation-registry { delegator: tx-sender })) ERR-DELEGATION-FAILED)
    
    (map-delete delegation-registry { delegator: tx-sender })
    (ok true)))

;; Read-only Functions
(define-read-only (get-stake (user principal))
  (map-get? stakes { user: user }))

(define-read-only (get-proposal (id uint))
  (map-get? proposals { id: id }))

(define-read-only (get-user-vote (proposal-id uint) (user principal))
  (map-get? user-votes { proposal-id: proposal-id, user: user }))

(define-read-only (get-treasury-balance)
  (var-get treasury-balance))

(define-read-only (get-total-staked)
  (var-get total-staked))

(define-read-only (get-protocol-status)
  (var-get protocol-active))

(define-read-only (get-delegation (delegator principal))
  (map-get? delegation-registry { delegator: delegator }))

(define-read-only (calculate-voting-power (user principal))
  (let (
    (user-stake (default-to 
      { amount: u0, duration: u0, start-block: u0, conviction-score: u0, participation-count: u0, last-activity: u0 }
      (map-get? stakes { user: user })))
  )
    (calculate-quadratic-voting-weight (get amount user-stake) (get conviction-score user-stake))))

(define-read-only (get-proposal-status (proposal-id uint))
  (let (
    (proposal (map-get? proposals { id: proposal-id }))
  )
    (match proposal
      prop {
        proposal-id: proposal-id,
        active: (and (<= block-height (get end-block prop)) (not (get executed prop))),
        ended: (> block-height (get end-block prop)),
        executed: (get executed prop),
        winning: (> (get votes-for prop) (get votes-against prop))
      }
      { proposal-id: proposal-id, active: false, ended: false, executed: false, winning: false })))

(define-read-only (get-next-proposal-id)
  (var-get next-proposal-id))