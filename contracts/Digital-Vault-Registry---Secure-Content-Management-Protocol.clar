;; Digital Vault Registry - Secure Content Management Protocol
;; 
;; Revolutionary decentralized system for managing digital content with cryptographic security
;; Advanced permission-based access control with comprehensive metadata tracking
;; Enterprise-level content authentication and ownership verification platform

;; Root authority for system configuration and governance operations
(define-constant system-root-authority tx-sender)

;; Specialized error handling codes for robust exception management
(define-constant error-content-missing (err u401))
(define-constant error-content-duplicate (err u402))
(define-constant error-invalid-input (err u403))
(define-constant error-size-limits (err u404))
(define-constant error-unauthorized (err u405))
(define-constant error-owner-mismatch (err u406))
(define-constant error-privilege-insufficient (err u400))
(define-constant error-access-forbidden (err u407))
(define-constant error-metadata-invalid (err u408))

;; Sequential identifier generator for unique content tracking
(define-data-var content-sequence-number uint u0)

;; Primary storage structure for digital content records with full metadata
(define-map digital-vault-storage
  { content-id: uint }
  {
    content-title: (string-ascii 64),
    content-proprietor: principal,
    data-volume: uint,
    registration-height: uint,
    content-summary: (string-ascii 128),
    classification-labels: (list 10 (string-ascii 32))
  }
)

;; Access control matrix for granular permission management
(define-map permission-control-matrix
  { content-id: uint, authorized-user: principal }
  { permission-status: bool }
)

;; ===== Internal utility functions for validation and verification =====

;; Label format verification ensuring proper string constraints
(define-private (validate-label-format (label-text (string-ascii 32)))
  (and
    (> (len label-text) u0)
    (< (len label-text) u33)
  )
)

;; Complete label collection validation with format checking
(define-private (verify-label-collection (label-set (list 10 (string-ascii 32))))
  (and
    (> (len label-set) u0)
    (<= (len label-set) u10)
    (is-eq (len (filter validate-label-format label-set)) (len label-set))
  )
)

;; Content record existence verification in vault storage
(define-private (check-content-exists (content-id uint))
  (is-some (map-get? digital-vault-storage { content-id: content-id }))
)

;; Data volume extraction with safe default handling
(define-private (extract-data-volume (content-id uint))
  (default-to u0
    (get data-volume
      (map-get? digital-vault-storage { content-id: content-id })
    )
  )
)

;; Ownership authentication comparing record owner with transaction sender
(define-private (authenticate-ownership (content-id uint) (requesting-user principal))
  (match (map-get? digital-vault-storage { content-id: content-id })
    content-record (is-eq (get content-proprietor content-record) requesting-user)
    false
  )
)

;; ===== Core management functions for content lifecycle operations =====


;; Comprehensive content registration with extensive input validation
(define-public (create-vault-entry
  (content-title (string-ascii 64))
  (data-volume uint)
  (content-summary (string-ascii 128))
  (classification-labels (list 10 (string-ascii 32)))
)
  (let
    (
      (next-content-id (+ (var-get content-sequence-number) u1))
    )
    ;; Thorough input validation with specific constraint enforcement
    (asserts! (> (len content-title) u0) error-invalid-input)
    (asserts! (< (len content-title) u65) error-invalid-input)
    (asserts! (> data-volume u0) error-size-limits)
    (asserts! (< data-volume u1000000000) error-size-limits)
    (asserts! (> (len content-summary) u0) error-invalid-input)
    (asserts! (< (len content-summary) u129) error-invalid-input)
    (asserts! (verify-label-collection classification-labels) error-metadata-invalid)

    ;; Register new content in digital vault storage
    (map-insert digital-vault-storage
      { content-id: next-content-id }
      {
        content-title: content-title,
        content-proprietor: tx-sender,
        data-volume: data-volume,
        registration-height: block-height,
        content-summary: content-summary,
        classification-labels: classification-labels
      }
    )

    ;; Establish initial ownership permissions for creator
    (map-insert permission-control-matrix
      { content-id: next-content-id, authorized-user: tx-sender }
      { permission-status: true }
    )

    ;; Increment sequence counter for future registrations
    (var-set content-sequence-number next-content-id)
    (ok next-content-id)
  )
)

;; Secure ownership transfer with authorization validation
(define-public (reassign-proprietorship (content-id uint) (successor-owner principal))
  (let
    (
      (content-record (unwrap! (map-get? digital-vault-storage { content-id: content-id })
        error-content-missing))
    )
    ;; Validate content existence and current ownership before transfer
    (asserts! (check-content-exists content-id) error-content-missing)
    (asserts! (is-eq (get content-proprietor content-record) tx-sender) error-owner-mismatch)

    ;; Execute ownership transfer by updating proprietor field
    (map-set digital-vault-storage
      { content-id: content-id }
      (merge content-record { content-proprietor: successor-owner })
    )
    (ok true)
  )
)

;; Advanced metadata modification with ownership verification
(define-public (modify-content-metadata
  (content-id uint)
  (updated-title (string-ascii 64))
  (updated-volume uint)
  (updated-summary (string-ascii 128))
  (updated-labels (list 10 (string-ascii 32)))
)
  (let
    (
      (existing-record (unwrap! (map-get? digital-vault-storage { content-id: content-id })
        error-content-missing))
    )
    ;; Comprehensive authorization and input validation
    (asserts! (check-content-exists content-id) error-content-missing)
    (asserts! (is-eq (get content-proprietor existing-record) tx-sender) error-owner-mismatch)
    (asserts! (> (len updated-title) u0) error-invalid-input)
    (asserts! (< (len updated-title) u65) error-invalid-input)
    (asserts! (> updated-volume u0) error-size-limits)
    (asserts! (< updated-volume u1000000000) error-size-limits)
    (asserts! (> (len updated-summary) u0) error-invalid-input)
    (asserts! (< (len updated-summary) u129) error-invalid-input)
    (asserts! (verify-label-collection updated-labels) error-metadata-invalid)

    ;; Apply comprehensive metadata updates to existing record
    (map-set digital-vault-storage
      { content-id: content-id }
      (merge existing-record {
        content-title: updated-title,
        data-volume: updated-volume,
        content-summary: updated-summary,
        classification-labels: updated-labels
      })
    )
    (ok true)
  )
)

;; Permanent content removal with ownership enforcement
(define-public (remove-vault-entry (content-id uint))
  (let
    (
      (target-record (unwrap! (map-get? digital-vault-storage { content-id: content-id })
        error-content-missing))
    )
    ;; Verify ownership before allowing permanent deletion
    (asserts! (check-content-exists content-id) error-content-missing)
    (asserts! (is-eq (get content-proprietor target-record) tx-sender) error-owner-mismatch)

    ;; Execute irreversible content removal from vault
    (map-delete digital-vault-storage { content-id: content-id })
    (ok true)
  )
)

;; ===== Information retrieval functions for data access and verification =====

;; Comprehensive permission status analysis for specific user and content
(define-read-only (analyze-access-rights (content-id uint) (target-user principal))
  (let
    (
      (content-record (unwrap! (map-get? digital-vault-storage { content-id: content-id })
        error-content-missing))
      (explicit-permission (default-to false
        (get permission-status
          (map-get? permission-control-matrix { content-id: content-id, authorized-user: target-user })
        )
      ))
    )
    ;; Return comprehensive access analysis with multiple permission checks
    (ok {
      has-direct-permission: explicit-permission,
      is-content-owner: (is-eq (get content-proprietor content-record) target-user),
      access-permitted: (or explicit-permission (is-eq (get content-proprietor content-record) target-user))
    })
  )
)

;; Proprietor information extraction for ownership verification
(define-read-only (retrieve-proprietor-data (content-id uint))
  (match (map-get? digital-vault-storage { content-id: content-id })
    content-record (ok (get content-proprietor content-record))
    error-content-missing
  )
)

;; Complete content information retrieval with access control enforcement
(define-read-only (fetch-content-details (content-id uint))
  (let
    (
      (content-record (unwrap! (map-get? digital-vault-storage { content-id: content-id })
        error-content-missing))
      (user-authorized (default-to false
        (get permission-status
          (map-get? permission-control-matrix { content-id: content-id, authorized-user: tx-sender })
        )
      ))
    )
    ;; Enforce access control before revealing sensitive information
    (asserts! (check-content-exists content-id) error-content-missing)
    (asserts! (or user-authorized (is-eq (get content-proprietor content-record) tx-sender)) error-access-forbidden)

    ;; Return complete content structure with all metadata
    (ok {
      content-title: (get content-title content-record),
      content-proprietor: (get content-proprietor content-record),
      data-volume: (get data-volume content-record),
      registration-height: (get registration-height content-record),
      content-summary: (get content-summary content-record),
      classification-labels: (get classification-labels content-record)
    })
  )
)

;; System statistics and administrative information retrieval
(define-read-only (fetch-system-metrics)
  (ok {
    total-content-entries: (var-get content-sequence-number),
    system-administrator: system-root-authority
  })
)