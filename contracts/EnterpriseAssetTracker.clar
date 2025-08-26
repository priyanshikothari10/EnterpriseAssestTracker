;; Enterprise Asset Tracker Contract
;; A smart contract for tracking corporate assets across locations with maintenance schedules and depreciation

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-asset-not-found (err u102))
(define-constant err-invalid-data (err u103))
(define-constant err-asset-already-exists (err u104))

;; Data structures
(define-map assets
  { asset-id: (string-ascii 50) }
  {
    name: (string-ascii 100),
    category: (string-ascii 50),
    location: (string-ascii 100),
    purchase-date: uint,
    purchase-value: uint,
    current-value: uint,
    last-maintenance: uint,
    next-maintenance: uint,
    status: (string-ascii 20),
    owner: principal
  }
)

(define-map authorized-managers principal bool)
(define-data-var total-assets uint u0)

;; Authorization functions
(define-private (is-authorized (user principal))
  (or 
    (is-eq user contract-owner)
    (default-to false (map-get? authorized-managers user))
  )
)

;; Function 1: Register Asset
;; Registers a new corporate asset with all tracking details
(define-public (register-asset 
  (asset-id (string-ascii 50))
  (name (string-ascii 100))
  (category (string-ascii 50))
  (location (string-ascii 100))
  (purchase-date uint)
  (purchase-value uint)
  (maintenance-interval uint))
  (begin
    ;; Check authorization
    (asserts! (is-authorized tx-sender) err-not-authorized)
    
    ;; Validate input data
    (asserts! (> (len asset-id) u0) err-invalid-data)
    (asserts! (> (len name) u0) err-invalid-data)
    (asserts! (> purchase-value u0) err-invalid-data)
    (asserts! (> purchase-date u0) err-invalid-data)
    
    ;; Check if asset already exists
    (asserts! (is-none (map-get? assets {asset-id: asset-id})) err-asset-already-exists)
    
    ;; Calculate next maintenance date (add interval in blocks to purchase date)
    (let ((next-maintenance-date (+ purchase-date maintenance-interval)))
      ;; Store asset information
      (map-set assets
        {asset-id: asset-id}
        {
          name: name,
          category: category,
          location: location,
          purchase-date: purchase-date,
          purchase-value: purchase-value,
          current-value: purchase-value, ;; Initially same as purchase value
          last-maintenance: u0,
          next-maintenance: next-maintenance-date,
          status: "active",
          owner: tx-sender
        })
      
      ;; Increment total assets counter
      (var-set total-assets (+ (var-get total-assets) u1))
      
      ;; Print event for tracking
      (print {
        event: "asset-registered",
        asset-id: asset-id,
        name: name,
        location: location,
        value: purchase-value
      })
      
      (ok asset-id)
    )
  )
)

;; Function 2: Update Asset Maintenance and Depreciation
;; Updates maintenance records and calculates depreciation
(define-public (update-asset-maintenance
  (asset-id (string-ascii 50))
  (new-location (optional (string-ascii 100)))
  (maintenance-performed bool)
  (depreciation-rate uint)) ;; Depreciation rate as percentage (e.g., 10 = 10%)
  (begin
    ;; Check authorization
    (asserts! (is-authorized tx-sender) err-not-authorized)
    
    ;; Get existing asset
    (let ((asset-data (unwrap! (map-get? assets {asset-id: asset-id}) err-asset-not-found)))
      
      ;; Calculate new depreciated value
      (let (
        (current-val (get current-value asset-data))
        (depreciation-amount (/ (* current-val depreciation-rate) u100))
        (new-value (if (> current-val depreciation-amount) 
                      (- current-val depreciation-amount) 
                      u1)) ;; Minimum value of 1
        (current-block stacks-block-height)
        (updated-location (match new-location
                            loc loc
                            (get location asset-data)))
      )
      
      ;; Update asset with new information
      (map-set assets
        {asset-id: asset-id}
        {
          name: (get name asset-data),
          category: (get category asset-data),
          location: updated-location,
          purchase-date: (get purchase-date asset-data),
          purchase-value: (get purchase-value asset-data),
          current-value: new-value,
          last-maintenance: (if maintenance-performed current-block (get last-maintenance asset-data)),
          next-maintenance: (if maintenance-performed 
                              (+ current-block u1000) ;; Next maintenance in ~1000 blocks
                              (get next-maintenance asset-data)),
          status: (get status asset-data),
          owner: (get owner asset-data)
        })
      
      ;; Print maintenance event
      (print {
        event: "asset-updated",
        asset-id: asset-id,
        maintenance-performed: maintenance-performed,
        new-value: new-value,
        location: updated-location
      })
      
      (ok {
        asset-id: asset-id,
        new-value: new-value,
        maintenance-updated: maintenance-performed
      })
      )
    )
  )
)

;; Read-only functions for querying asset data
(define-read-only (get-asset (asset-id (string-ascii 50)))
  (map-get? assets {asset-id: asset-id}))

(define-read-only (get-total-assets)
  (var-get total-assets))

;; Owner function to authorize managers
(define-public (authorize-manager (manager principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-managers manager true)
    (ok true)))

(define-public (revoke-manager (manager principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-managers manager false)
    (ok true)))