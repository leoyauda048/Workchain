;; Contract Amendment System for Workchain
;; Allows both parties to propose and approve modifications to existing employment contracts

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-INVALID-PARAMS (err u201))
(define-constant ERR-CONTRACT-NOT-FOUND (err u202))
(define-constant ERR-AMENDMENT-NOT-FOUND (err u203))
(define-constant ERR-AMENDMENT-ALREADY-PROCESSED (err u204))
(define-constant ERR-CANNOT-AMEND-INACTIVE (err u205))
(define-constant ERR-SELF-APPROVAL-NOT-ALLOWED (err u206))
(define-constant ERR-ALREADY-APPROVED (err u207))
(define-constant ERR-ALREADY-REJECTED (err u208))

;; Data variables
(define-data-var amendment-id-nonce uint u0)

;; Amendment types enum
(define-constant AMENDMENT-SALARY u1)
(define-constant AMENDMENT-TITLE u2)
(define-constant AMENDMENT-DUTIES u3)
(define-constant AMENDMENT-BENEFITS u4)
(define-constant AMENDMENT-END-DATE u5)

;; Amendment status enum
(define-constant STATUS-PENDING u0)
(define-constant STATUS-APPROVED u1)
(define-constant STATUS-REJECTED u2)
(define-constant STATUS-APPLIED u3)

;; Data maps
(define-map contract-amendments
    uint
    {
        contract-id: uint,
        amendment-type: uint,
        proposer: principal,
        proposed-value: (string-ascii 500),
        proposed-uint-value: (optional uint),
        reason: (string-ascii 300),
        status: uint,
        employer-approved: bool,
        employee-approved: bool,
        created-at: uint,
        processed-at: (optional uint)
    }
)

(define-map contract-amendment-history
    uint
    (list 50 uint)
)

;; Import employment contracts from main contract
(define-trait employment-contract-trait
    (
        (get-contract (uint) (response (tuple (employer principal) (employee principal) (title (string-ascii 50)) 
            (salary uint) (start-date uint) (end-date uint) (duties (string-ascii 500)) 
            (benefits (string-ascii 200)) (signed-by-employer bool) (signed-by-employee bool) (active bool)) uint))
    )
)

;; Public functions
(define-public (propose-amendment
    (contract-id uint)
    (amendment-type uint)
    (proposed-value (string-ascii 500))
    (proposed-uint-value (optional uint))
    (reason (string-ascii 300)))
    (let
        (
            (amendment-id (var-get amendment-id-nonce))
            (contract (unwrap! (contract-call? .Workchain get-contract contract-id) ERR-CONTRACT-NOT-FOUND))
            (current-amendments (default-to (list) (map-get? contract-amendment-history contract-id)))
        )
        ;; Validate contract exists and caller is authorized
        (asserts! (or 
            (is-eq (get employer contract) tx-sender)
            (is-eq (get employee contract) tx-sender)
        ) ERR-NOT-AUTHORIZED)
        (asserts! (get active contract) ERR-CANNOT-AMEND-INACTIVE)
        (asserts! (and (>= amendment-type u1) (<= amendment-type u5)) ERR-INVALID-PARAMS)
        
        ;; Validate type-specific requirements
        (if (is-eq amendment-type AMENDMENT-SALARY)
            (asserts! (is-some proposed-uint-value) ERR-INVALID-PARAMS)
            (asserts! (> (len proposed-value) u0) ERR-INVALID-PARAMS)
        )
        
        ;; Create amendment proposal
        (map-set contract-amendments amendment-id {
            contract-id: contract-id,
            amendment-type: amendment-type,
            proposer: tx-sender,
            proposed-value: proposed-value,
            proposed-uint-value: proposed-uint-value,
            reason: reason,
            status: STATUS-PENDING,
            employer-approved: (is-eq (get employer contract) tx-sender),
            employee-approved: (is-eq (get employee contract) tx-sender),
            created-at: stacks-block-height,
            processed-at: none
        })
        
        ;; Add to contract's amendment history
        (map-set contract-amendment-history 
            contract-id 
            (unwrap! (as-max-len? (append current-amendments amendment-id) u50) ERR-INVALID-PARAMS))
        
        (var-set amendment-id-nonce (+ amendment-id u1))
        (ok amendment-id)
    )
)

(define-public (approve-amendment (amendment-id uint))
    (let
        (
            (amendment (unwrap! (map-get? contract-amendments amendment-id) ERR-AMENDMENT-NOT-FOUND))
            (contract (unwrap! (contract-call? .Workchain get-contract (get contract-id amendment)) ERR-CONTRACT-NOT-FOUND))
        )
        ;; Validate caller can approve
        (asserts! (or 
            (is-eq (get employer contract) tx-sender)
            (is-eq (get employee contract) tx-sender)
        ) ERR-NOT-AUTHORIZED)
        (asserts! (not (is-eq (get proposer amendment) tx-sender)) ERR-SELF-APPROVAL-NOT-ALLOWED)
        (asserts! (is-eq (get status amendment) STATUS-PENDING) ERR-AMENDMENT-ALREADY-PROCESSED)
        
        ;; Check if already approved by this party
        (if (is-eq (get employer contract) tx-sender)
            (asserts! (not (get employer-approved amendment)) ERR-ALREADY-APPROVED)
            (asserts! (not (get employee-approved amendment)) ERR-ALREADY-APPROVED)
        )
        
        ;; Update approval status
        (let
            (
                (updated-amendment (merge amendment {
                    employer-approved: (or (get employer-approved amendment) (is-eq (get employer contract) tx-sender)),
                    employee-approved: (or (get employee-approved amendment) (is-eq (get employee contract) tx-sender))
                }))
            )
            (map-set contract-amendments amendment-id updated-amendment)
            
            ;; Check if fully approved and apply if so
            (if (and (get employer-approved updated-amendment) (get employee-approved updated-amendment))
                (begin
                    (try! (apply-amendment amendment-id))
                    (ok true)
                )
                (ok true)
            )
        )
    )
)

(define-public (reject-amendment (amendment-id uint))
    (let
        (
            (amendment (unwrap! (map-get? contract-amendments amendment-id) ERR-AMENDMENT-NOT-FOUND))
            (contract (unwrap! (contract-call? .Workchain get-contract (get contract-id amendment)) ERR-CONTRACT-NOT-FOUND))
        )
        ;; Validate caller can reject
        (asserts! (or 
            (is-eq (get employer contract) tx-sender)
            (is-eq (get employee contract) tx-sender)
        ) ERR-NOT-AUTHORIZED)
        (asserts! (not (is-eq (get proposer amendment) tx-sender)) ERR-SELF-APPROVAL-NOT-ALLOWED)
        (asserts! (is-eq (get status amendment) STATUS-PENDING) ERR-AMENDMENT-ALREADY-PROCESSED)
        
        ;; Mark as rejected
        (map-set contract-amendments amendment-id 
            (merge amendment {
                status: STATUS-REJECTED,
                processed-at: (some stacks-block-height)
            })
        )
        (ok true)
    )
)

;; Private function to apply approved amendment
(define-private (apply-amendment (amendment-id uint))
    (let
        (
            (amendment (unwrap! (map-get? contract-amendments amendment-id) ERR-AMENDMENT-NOT-FOUND))
        )
        ;; Mark amendment as applied
        (map-set contract-amendments amendment-id 
            (merge amendment {
                status: STATUS-APPLIED,
                processed-at: (some stacks-block-height)
            })
        )
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-amendment (amendment-id uint))
    (ok (unwrap! (map-get? contract-amendments amendment-id) ERR-AMENDMENT-NOT-FOUND))
)

(define-read-only (get-contract-amendments (contract-id uint))
    (ok (default-to (list) (map-get? contract-amendment-history contract-id)))
)

(define-read-only (get-pending-amendments (contract-id uint))
    (let
        (
            (amendments (default-to (list) (map-get? contract-amendment-history contract-id)))
        )
        (ok (filter is-amendment-pending amendments))
    )
)

(define-read-only (is-amendment-pending (amendment-id uint))
    (match (map-get? contract-amendments amendment-id)
        amendment (is-eq (get status amendment) STATUS-PENDING)
        false
    )
)

(define-read-only (get-amendment-status (amendment-id uint))
    (let
        (
            (amendment (unwrap! (map-get? contract-amendments amendment-id) ERR-AMENDMENT-NOT-FOUND))
        )
        (ok {
            status: (get status amendment),
            employer-approved: (get employer-approved amendment),
            employee-approved: (get employee-approved amendment),
            fully-approved: (and (get employer-approved amendment) (get employee-approved amendment))
        })
    )
)

(define-read-only (get-amendment-type-name (amendment-type uint))
    (ok (if (is-eq amendment-type AMENDMENT-SALARY) "Salary"
        (if (is-eq amendment-type AMENDMENT-TITLE) "Title"
        (if (is-eq amendment-type AMENDMENT-DUTIES) "Duties"
        (if (is-eq amendment-type AMENDMENT-BENEFITS) "Benefits"
        (if (is-eq amendment-type AMENDMENT-END-DATE) "End Date"
        "Unknown"
    ))))))
)
