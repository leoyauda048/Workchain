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

(define-data-var contract-id-nonce uint u0)
(define-data-var payment-id-nonce uint u0)
(define-data-var dispute-id-nonce uint u0)

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