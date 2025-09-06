(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PARAMS (err u101))
(define-constant ERR-CONTRACT-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-SIGNED (err u103))
(define-constant ERR-NOT-SIGNED (err u104))
(define-constant ERR-EXPIRED (err u105))
(define-constant ERR-INSUFFICIENT-FUNDS (err u106))
(define-constant ERR-PAYMENT-NOT-FOUND (err u107))
(define-constant ERR-PAYMENT-ALREADY-RELEASED (err u108))
(define-constant ERR-DISPUTE-NOT-FOUND (err u109))
(define-constant ERR-PAYMENT-DISPUTED (err u110))
(define-constant ERR-DISPUTE-ALREADY-EXISTS (err u111))
(define-constant ERR-UNAUTHORIZED-ARBITRATOR (err u112))
(define-constant ERR-REVIEW-NOT-FOUND (err u113))
(define-constant ERR-REVIEW-ALREADY-SUBMITTED (err u114))
(define-constant ERR-INVALID-RATING (err u115))
(define-constant ERR-REVIEW-PERIOD-ACTIVE (err u116))
(define-constant ERR-EMPLOYEE-RESPONSE-EXISTS (err u117))

(define-data-var contract-id-nonce uint u0)
(define-data-var payment-id-nonce uint u0)
(define-data-var dispute-id-nonce uint u0)
(define-data-var review-id-nonce uint u0)

(define-map employment-contracts
    uint
    {
        employer: principal,
        employee: principal,
        title: (string-ascii 50),
        salary: uint,
        start-date: uint,
        end-date: uint,
        duties: (string-ascii 500),
        benefits: (string-ascii 200),
        signed-by-employer: bool,
        signed-by-employee: bool,
        active: bool
    }
)

(define-map employee-contracts
    principal
    (list 20 uint)
)

(define-map employer-contracts
    principal
    (list 20 uint)
)

(define-map payment-escrows
    uint
    {
        contract-id: uint,
        payer: principal,
        payee: principal,
        amount: uint,
        period-start: uint,
        period-end: uint,
        description: (string-ascii 200),
        released: bool,
        disputed: bool,
        created-at: uint
    }
)

(define-map contract-payments
    uint
    (list 50 uint)
)

(define-map disputes
    uint
    {
        payment-id: uint,
        initiator: principal,
        reason: (string-ascii 300),
        arbitrator: (optional principal),
        resolved: bool,
        resolution: (optional (string-ascii 300)),
        created-at: uint
    }
)

(define-map arbitrator-pool
    principal
    bool
)

(define-map performance-reviews
    uint
    {
        contract-id: uint,
        employer: principal,
        employee: principal,
        review-period-start: uint,
        review-period-end: uint,
        overall-rating: uint,
        productivity-rating: uint,
        quality-rating: uint,
        communication-rating: uint,
        reliability-rating: uint,
        employer-feedback: (string-ascii 500),
        submitted: bool,
        submitted-at: uint
    }
)

(define-map employee-responses
    uint
    {
        review-id: uint,
        employee: principal,
        self-assessment: (string-ascii 500),
        goals: (string-ascii 300),
        concerns: (string-ascii 300),
        submitted-at: uint
    }
)

(define-map contract-reviews
    uint
    (list 30 uint)
)

(define-map employee-performance-history
    principal
    {
        total-reviews: uint,
        average-overall-rating: uint,
        average-productivity-rating: uint,
        average-quality-rating: uint,
        average-communication-rating: uint,
        average-reliability-rating: uint,
        last-review-date: uint
    }
)

(define-public (create-contract
    (employee principal)
    (title (string-ascii 50))
    (salary uint)
    (start-date uint)
    (end-date uint)
    (duties (string-ascii 500))
    (benefits (string-ascii 200)))
    (let
        (
            (contract-id (var-get contract-id-nonce))
            (employer tx-sender)
            (employer-contracts-list (default-to (list) (map-get? employer-contracts employer)))
        )
        (asserts! (> end-date start-date) ERR-INVALID-PARAMS)
        (asserts! (> salary u0) ERR-INVALID-PARAMS)
        
        (map-set employment-contracts contract-id {
            employer: employer,
            employee: employee,
            title: title,
            salary: salary,
            start-date: start-date,
            end-date: end-date,
            duties: duties,
            benefits: benefits,
            signed-by-employer: true,
            signed-by-employee: false,
            active: false
        })
        
        (map-set employer-contracts 
            employer 
            (unwrap! (as-max-len? (append employer-contracts-list contract-id) u20) ERR-INVALID-PARAMS))
        
        (var-set contract-id-nonce (+ contract-id u1))
        (ok contract-id)
    )
)

(define-public (sign-contract (contract-id uint))
    (let (
        (contract (unwrap! (map-get? employment-contracts contract-id) ERR-CONTRACT-NOT-FOUND))
        (employee-contracts-list (default-to (list) (map-get? employee-contracts tx-sender)))
    )
        (asserts! (is-eq (get employee contract) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (not (get signed-by-employee contract)) ERR-ALREADY-SIGNED)
        
        (map-set employment-contracts contract-id 
            (merge contract {
                signed-by-employee: true,
                active: true
            })
        )
        
        (map-set employee-contracts 
            tx-sender 
            (unwrap! (as-max-len? (append employee-contracts-list contract-id) u20) ERR-INVALID-PARAMS))
        
        (ok true)
    )
)

(define-public (terminate-contract (contract-id uint))
    (let (
        (contract (unwrap! (map-get? employment-contracts contract-id) ERR-CONTRACT-NOT-FOUND))
    )
        (asserts! (or 
            (is-eq (get employer contract) tx-sender)
            (is-eq (get employee contract) tx-sender)
        ) ERR-NOT-AUTHORIZED)
        
        (map-set employment-contracts contract-id 
            (merge contract {
                active: false
            })
        )
        (ok true)
    )
)

(define-read-only (get-contract (contract-id uint))
    (ok (unwrap! (map-get? employment-contracts contract-id) ERR-CONTRACT-NOT-FOUND))
)

(define-read-only (get-employee-contracts (employee principal))
    (ok (default-to (list) (map-get? employee-contracts employee)))
)

(define-read-only (get-employer-contracts (employer principal))
    (ok (default-to (list) (map-get? employer-contracts employer)))
)

(define-read-only (is-contract-active (contract-id uint))
    (let (
        (contract (unwrap! (map-get? employment-contracts contract-id) ERR-CONTRACT-NOT-FOUND))
    )
        (ok (get active contract))
    )
)

(define-public (deposit-payment-escrow
    (contract-id uint)
    (amount uint)
    (period-start uint)
    (period-end uint)
    (description (string-ascii 200)))
    (let
        (
            (payment-id (var-get payment-id-nonce))
            (contract (unwrap! (map-get? employment-contracts contract-id) ERR-CONTRACT-NOT-FOUND))
            (current-payments (default-to (list) (map-get? contract-payments contract-id)))
        )
        (asserts! (is-eq (get employer contract) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get active contract) ERR-CONTRACT-NOT-FOUND)
        (asserts! (> amount u0) ERR-INVALID-PARAMS)
        (asserts! (> period-end period-start) ERR-INVALID-PARAMS)
        (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-FUNDS)
        
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        (map-set payment-escrows payment-id {
            contract-id: contract-id,
            payer: tx-sender,
            payee: (get employee contract),
            amount: amount,
            period-start: period-start,
            period-end: period-end,
            description: description,
            released: false,
            disputed: false,
            created-at: stacks-block-height
        })
        
        (map-set contract-payments 
            contract-id 
            (unwrap! (as-max-len? (append current-payments payment-id) u50) ERR-INVALID-PARAMS))
        
        (var-set payment-id-nonce (+ payment-id u1))
        (ok payment-id)
    )
)

(define-public (release-payment (payment-id uint))
    (let (
        (payment (unwrap! (map-get? payment-escrows payment-id) ERR-PAYMENT-NOT-FOUND))
    )
        (asserts! (or 
            (is-eq (get payer payment) tx-sender)
            (is-eq (get payee payment) tx-sender)
        ) ERR-NOT-AUTHORIZED)
        (asserts! (not (get released payment)) ERR-PAYMENT-ALREADY-RELEASED)
        (asserts! (not (get disputed payment)) ERR-PAYMENT-DISPUTED)
        
        (try! (as-contract (stx-transfer? (get amount payment) tx-sender (get payee payment))))
        
        (map-set payment-escrows payment-id 
            (merge payment {
                released: true
            })
        )
        (ok true)
    )
)

(define-public (claim-payment (payment-id uint))
    (let (
        (payment (unwrap! (map-get? payment-escrows payment-id) ERR-PAYMENT-NOT-FOUND))
    )
        (asserts! (is-eq (get payee payment) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (not (get released payment)) ERR-PAYMENT-ALREADY-RELEASED)
        (asserts! (not (get disputed payment)) ERR-PAYMENT-DISPUTED)
        (asserts! (>= stacks-block-height (get period-end payment)) ERR-INVALID-PARAMS)
        
        (try! (as-contract (stx-transfer? (get amount payment) tx-sender (get payee payment))))
        
        (map-set payment-escrows payment-id 
            (merge payment {
                released: true
            })
        )
        (ok true)
    )
)

(define-public (create-dispute
    (payment-id uint)
    (reason (string-ascii 300)))
    (let
        (
            (dispute-id (var-get dispute-id-nonce))
            (payment (unwrap! (map-get? payment-escrows payment-id) ERR-PAYMENT-NOT-FOUND))
        )
        (asserts! (or 
            (is-eq (get payer payment) tx-sender)
            (is-eq (get payee payment) tx-sender)
        ) ERR-NOT-AUTHORIZED)
        (asserts! (not (get released payment)) ERR-PAYMENT-ALREADY-RELEASED)
        (asserts! (not (get disputed payment)) ERR-DISPUTE-ALREADY-EXISTS)
        
        (map-set payment-escrows payment-id 
            (merge payment {
                disputed: true
            })
        )
        
        (map-set disputes dispute-id {
            payment-id: payment-id,
            initiator: tx-sender,
            reason: reason,
            arbitrator: none,
            resolved: false,
            resolution: none,
            created-at: stacks-block-height
        })
        
        (var-set dispute-id-nonce (+ dispute-id u1))
        (ok dispute-id)
    )
)

(define-public (assign-arbitrator
    (dispute-id uint)
    (arbitrator principal))
    (let (
        (dispute (unwrap! (map-get? disputes dispute-id) ERR-DISPUTE-NOT-FOUND))
        (payment (unwrap! (map-get? payment-escrows (get payment-id dispute)) ERR-PAYMENT-NOT-FOUND))
    )
        (asserts! (is-eq (get payer payment) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (not (get resolved dispute)) ERR-DISPUTE-NOT-FOUND)
        (asserts! (default-to false (map-get? arbitrator-pool arbitrator)) ERR-UNAUTHORIZED-ARBITRATOR)
        
        (map-set disputes dispute-id 
            (merge dispute {
                arbitrator: (some arbitrator)
            })
        )
        (ok true)
    )
)

(define-public (resolve-dispute
    (dispute-id uint)
    (resolution (string-ascii 300))
    (release-to-payee bool))
    (let (
        (dispute (unwrap! (map-get? disputes dispute-id) ERR-DISPUTE-NOT-FOUND))
        (payment (unwrap! (map-get? payment-escrows (get payment-id dispute)) ERR-PAYMENT-NOT-FOUND))
        (arbitrator (unwrap! (get arbitrator dispute) ERR-UNAUTHORIZED-ARBITRATOR))
    )
        (asserts! (is-eq arbitrator tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (not (get resolved dispute)) ERR-DISPUTE-NOT-FOUND)
        
        (if release-to-payee
            (try! (as-contract (stx-transfer? (get amount payment) tx-sender (get payee payment))))
            (try! (as-contract (stx-transfer? (get amount payment) tx-sender (get payer payment))))
        )
        
        (map-set payment-escrows (get payment-id dispute)
            (merge payment {
                released: true,
                disputed: false
            })
        )
        
        (map-set disputes dispute-id 
            (merge dispute {
                resolved: true,
                resolution: (some resolution)
            })
        )
        (ok true)
    )
)

(define-public (add-arbitrator (arbitrator principal))
    (begin
        (map-set arbitrator-pool arbitrator true)
        (ok true)
    )
)

(define-public (remove-arbitrator (arbitrator principal))
    (begin
        (map-delete arbitrator-pool arbitrator)
        (ok true)
    )
)

(define-read-only (get-payment-escrow (payment-id uint))
    (ok (unwrap! (map-get? payment-escrows payment-id) ERR-PAYMENT-NOT-FOUND))
)

(define-read-only (get-contract-payments (contract-id uint))
    (ok (default-to (list) (map-get? contract-payments contract-id)))
)

(define-read-only (get-dispute (dispute-id uint))
    (ok (unwrap! (map-get? disputes dispute-id) ERR-DISPUTE-NOT-FOUND))
)

(define-read-only (is-arbitrator (address principal))
    (ok (default-to false (map-get? arbitrator-pool address)))
)

(define-read-only (get-payment-status (payment-id uint))
    (let (
        (payment (unwrap! (map-get? payment-escrows payment-id) ERR-PAYMENT-NOT-FOUND))
    )
        (ok {
            released: (get released payment),
            disputed: (get disputed payment),
            amount: (get amount payment),
            can-claim: (and 
                (not (get released payment))
                (not (get disputed payment))
                (>= stacks-block-height (get period-end payment))
            )
        })
    )
)

(define-public (create-performance-review
    (contract-id uint)
    (review-period-start uint)
    (review-period-end uint))
    (let
        (
            (review-id (var-get review-id-nonce))
            (contract (unwrap! (map-get? employment-contracts contract-id) ERR-CONTRACT-NOT-FOUND))
            (current-reviews (default-to (list) (map-get? contract-reviews contract-id)))
        )
        (asserts! (is-eq (get employer contract) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get active contract) ERR-CONTRACT-NOT-FOUND)
        (asserts! (> review-period-end review-period-start) ERR-INVALID-PARAMS)
        (asserts! (>= review-period-start (get start-date contract)) ERR-INVALID-PARAMS)
        
        (map-set performance-reviews review-id {
            contract-id: contract-id,
            employer: tx-sender,
            employee: (get employee contract),
            review-period-start: review-period-start,
            review-period-end: review-period-end,
            overall-rating: u0,
            productivity-rating: u0,
            quality-rating: u0,
            communication-rating: u0,
            reliability-rating: u0,
            employer-feedback: "",
            submitted: false,
            submitted-at: u0
        })
        
        (map-set contract-reviews 
            contract-id 
            (unwrap! (as-max-len? (append current-reviews review-id) u30) ERR-INVALID-PARAMS))
        
        (var-set review-id-nonce (+ review-id u1))
        (ok review-id)
    )
)

(define-public (submit-performance-rating
    (review-id uint)
    (overall-rating uint)
    (productivity-rating uint)
    (quality-rating uint)
    (communication-rating uint)
    (reliability-rating uint)
    (employer-feedback (string-ascii 500)))
    (let (
        (review (unwrap! (map-get? performance-reviews review-id) ERR-REVIEW-NOT-FOUND))
    )
        (asserts! (is-eq (get employer review) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (not (get submitted review)) ERR-REVIEW-ALREADY-SUBMITTED)
        (asserts! (and (<= overall-rating u5) (>= overall-rating u1)) ERR-INVALID-RATING)
        (asserts! (and (<= productivity-rating u5) (>= productivity-rating u1)) ERR-INVALID-RATING)
        (asserts! (and (<= quality-rating u5) (>= quality-rating u1)) ERR-INVALID-RATING)
        (asserts! (and (<= communication-rating u5) (>= communication-rating u1)) ERR-INVALID-RATING)
        (asserts! (and (<= reliability-rating u5) (>= reliability-rating u1)) ERR-INVALID-RATING)
        
        (map-set performance-reviews review-id 
            (merge review {
                overall-rating: overall-rating,
                productivity-rating: productivity-rating,
                quality-rating: quality-rating,
                communication-rating: communication-rating,
                reliability-rating: reliability-rating,
                employer-feedback: employer-feedback,
                submitted: true,
                submitted-at: stacks-block-height
            })
        )
        
        (try! (update-employee-performance-history (get employee review) review-id))
        (ok true)
    )
)

(define-public (submit-employee-response
    (review-id uint)
    (self-assessment (string-ascii 500))
    (goals (string-ascii 300))
    (concerns (string-ascii 300)))
    (let
        (
            (review (unwrap! (map-get? performance-reviews review-id) ERR-REVIEW-NOT-FOUND))
            (existing-response (map-get? employee-responses review-id))
        )
        (asserts! (is-eq (get employee review) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get submitted review) ERR-REVIEW-NOT-FOUND)
        (asserts! (is-none existing-response) ERR-EMPLOYEE-RESPONSE-EXISTS)
        
        (map-set employee-responses review-id {
            review-id: review-id,
            employee: tx-sender,
            self-assessment: self-assessment,
            goals: goals,
            concerns: concerns,
            submitted-at: stacks-block-height
        })
        
        (ok true)
    )
)

(define-private (update-employee-performance-history (employee principal) (review-id uint))
    (let
        (
            (review (unwrap! (map-get? performance-reviews review-id) ERR-REVIEW-NOT-FOUND))
            (current-history (default-to {
                total-reviews: u0,
                average-overall-rating: u0,
                average-productivity-rating: u0,
                average-quality-rating: u0,
                average-communication-rating: u0,
                average-reliability-rating: u0,
                last-review-date: u0
            } (map-get? employee-performance-history employee)))
            (new-total (+ (get total-reviews current-history) u1))
            (current-total (get total-reviews current-history))
        )
        (map-set employee-performance-history employee {
            total-reviews: new-total,
            average-overall-rating: (/ (+ (* (get average-overall-rating current-history) current-total) (get overall-rating review)) new-total),
            average-productivity-rating: (/ (+ (* (get average-productivity-rating current-history) current-total) (get productivity-rating review)) new-total),
            average-quality-rating: (/ (+ (* (get average-quality-rating current-history) current-total) (get quality-rating review)) new-total),
            average-communication-rating: (/ (+ (* (get average-communication-rating current-history) current-total) (get communication-rating review)) new-total),
            average-reliability-rating: (/ (+ (* (get average-reliability-rating current-history) current-total) (get reliability-rating review)) new-total),
            last-review-date: (get submitted-at review)
        })
        
        (ok true)
    )
)

(define-public (close-review-period (review-id uint))
    (let (
        (review (unwrap! (map-get? performance-reviews review-id) ERR-REVIEW-NOT-FOUND))
    )
        (asserts! (is-eq (get employer review) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (>= stacks-block-height (get review-period-end review)) ERR-REVIEW-PERIOD-ACTIVE)
        
        (if (not (get submitted review))
            (map-set performance-reviews review-id 
                (merge review {
                    overall-rating: u3,
                    productivity-rating: u3,
                    quality-rating: u3,
                    communication-rating: u3,
                    reliability-rating: u3,
                    employer-feedback: "No rating submitted - defaulted to neutral",
                    submitted: true,
                    submitted-at: stacks-block-height
                })
            )
            true
        )
        
        (ok true)
    )
)

(define-read-only (get-performance-review (review-id uint))
    (ok (unwrap! (map-get? performance-reviews review-id) ERR-REVIEW-NOT-FOUND))
)

(define-read-only (get-employee-response (review-id uint))
    (ok (map-get? employee-responses review-id))
)

(define-read-only (get-contract-reviews (contract-id uint))
    (ok (default-to (list) (map-get? contract-reviews contract-id)))
)

(define-read-only (get-employee-performance-history (employee principal))
    (ok (map-get? employee-performance-history employee))
)

(define-read-only (get-employee-average-rating (employee principal))
    (let (
        (history (map-get? employee-performance-history employee))
    )
        (ok (match history
            some-history (get average-overall-rating some-history)
            u0
        ))
    )
)

(define-read-only (calculate-performance-score (employee principal))
    (let (
        (history (map-get? employee-performance-history employee))
    )
        (ok (match history
            some-history (/ (+ 
                (get average-overall-rating some-history)
                (get average-productivity-rating some-history)
                (get average-quality-rating some-history)
                (get average-communication-rating some-history)
                (get average-reliability-rating some-history)
            ) u5)
            u0
        ))
    )
)

(define-read-only (is-review-pending (review-id uint))
    (let (
        (review (unwrap! (map-get? performance-reviews review-id) ERR-REVIEW-NOT-FOUND))
    )
        (ok (and 
            (not (get submitted review))
            (< stacks-block-height (get review-period-end review))
        ))
    )
)


