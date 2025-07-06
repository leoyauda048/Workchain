(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PARAMS (err u101))
(define-constant ERR-CONTRACT-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-SIGNED (err u103))
(define-constant ERR-NOT-SIGNED (err u104))
(define-constant ERR-EXPIRED (err u105))

(define-data-var contract-id-nonce uint u0)

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