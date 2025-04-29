;; Global Expertise Network Hub (GENH)
;; A decentralized platform connecting talented individuals with global opportunities


;; ================= DATA STRUCTURES =================


;; Repository of organizational entities
(define-map organization-registry
    principal
    {
        title: (string-ascii 100),
        industry: (string-ascii 50),
        location: (string-ascii 100)
    }
)

;; Repository of personal capability records
(define-map individual-records
    principal
    {
        identity: (string-ascii 100),
        capabilities: (list 10 (string-ascii 50)),
        location: (string-ascii 100),
        history: (string-ascii 500)
    }
)


;; Repository of available positions
(define-map position-listings
    principal
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        publisher: principal,
        location: (string-ascii 100),
        requirements: (list 10 (string-ascii 50))
    }
)

;; ================= CONSTANT DEFINITIONS =================

;; System response codes for various operations
(define-constant RESPONSE-NOT-FOUND (err u404))
(define-constant RESPONSE-ALREADY-EXISTS (err u409))
(define-constant RESPONSE-INVALID-HISTORY (err u402))
(define-constant RESPONSE-INVALID-POSITION (err u403))
(define-constant RESPONSE-INVALID-CAPABILITIES (err u400))
(define-constant RESPONSE-INVALID-LOCATION (err u401))
(define-constant RESPONSE-ENTITY-MISSING (err u404))

;; ================= PUBLIC FUNCTIONS: ORGANIZATION OPERATIONS =================

;; Register a new organizational entity
(define-public (register-organization 
    (title (string-ascii 100))
    (industry (string-ascii 50))
    (location (string-ascii 100)))
    (let
        (
            (entity-owner tx-sender)
            (existing-entity (map-get? organization-registry entity-owner))
        )
        ;; Verify organization doesn't already exist
        (if (is-none existing-entity)
            (begin
                ;; Validate mandatory fields
                (if (or (is-eq title "")
                        (is-eq industry "")
                        (is-eq location ""))
                    (err RESPONSE-INVALID-LOCATION)
                    (begin
                        ;; Store the organization profile
                        (map-set organization-registry entity-owner
                            {
                                title: title,
                                industry: industry,
                                location: location
                            }
                        )
                        (ok "Organization successfully registered in the network.")
                    )
                )
            )
            (err RESPONSE-ALREADY-EXISTS)
        )
    )
)

;; Modify an existing organization profile
(define-public (modify-organization 
    (title (string-ascii 100))
    (industry (string-ascii 50))
    (location (string-ascii 100)))
    (let
        (
            (entity-owner tx-sender)
            (existing-entity (map-get? organization-registry entity-owner))
        )
        ;; Confirm organization exists
        (if (is-some existing-entity)
            (begin
                ;; Ensure all fields have valid content
                (if (or (is-eq title "")
                        (is-eq industry "")
                        (is-eq location ""))
                    (err RESPONSE-INVALID-LOCATION)
                    (begin
                        ;; Update organization details
                        (map-set organization-registry entity-owner
                            {
                                title: title,
                                industry: industry,
                                location: location
                            }
                        )
                        (ok "Organization profile successfully updated in the registry.")
                    )
                )
            )
            (err RESPONSE-ENTITY-MISSING)
        )
    )
)

;; Remove organization from the registry
(define-public (deregister-organization)
    (let
        (
            (entity-owner tx-sender)
            (existing-entity (map-get? organization-registry entity-owner))
        )
        ;; Verify organization exists
        (if (is-some existing-entity)
            (begin
                ;; Remove organization record
                (map-delete organization-registry entity-owner)
                (ok "Organization successfully removed from the registry.")
            )
            (err RESPONSE-ENTITY-MISSING)
        )
    )
)

;; ================= PUBLIC FUNCTIONS: POSITION OPERATIONS =================

;; Publish a new position
(define-public (publish-position 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (location (string-ascii 100))
    (requirements (list 10 (string-ascii 50))))
    (let
        (
            (entity-owner tx-sender)
            (existing-listing (map-get? position-listings entity-owner))
        )
        ;; Check if position already exists
        (if (is-none existing-listing)
            (begin
                ;; Validate all mandatory fields
                (if (or (is-eq title "")
                        (is-eq description "")
                        (is-eq location "")
                        (is-eq (len requirements) u0))
                    (err RESPONSE-INVALID-POSITION)
                    (begin
                        ;; Record the position
                        (map-set position-listings entity-owner
                            {
                                title: title,
                                description: description,
                                publisher: entity-owner,
                                location: location,
                                requirements: requirements
                            }
                        )
                        (ok "Position successfully published to the network.")
                    )
                )
            )
            (err RESPONSE-ALREADY-EXISTS)
        )
    )
)

;; Update an existing position listing
(define-public (update-position 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (location (string-ascii 100))
    (requirements (list 10 (string-ascii 50))))
    (let
        (
            (entity-owner tx-sender)
            (existing-listing (map-get? position-listings entity-owner))
        )
        ;; Verify position exists
        (if (is-some existing-listing)
            (begin
                ;; Ensure all fields have valid content
                (if (or (is-eq title "")
                        (is-eq description "")
                        (is-eq location "")
                        (is-eq (len requirements) u0))
                    (err RESPONSE-INVALID-POSITION)
                    (begin
                        ;; Update position details
                        (map-set position-listings entity-owner
                            {
                                title: title,
                                description: description,
                                publisher: entity-owner,
                                location: location,
                                requirements: requirements
                            }
                        )
                        (ok "Position listing successfully updated.")
                    )
                )
            )
            (err RESPONSE-ENTITY-MISSING)
        )
    )
)

;; Remove a position listing
(define-public (withdraw-position)
    (let
        (
            (entity-owner tx-sender)
            (existing-listing (map-get? position-listings entity-owner))
        )
        ;; Check if position exists
        (if (is-some existing-listing)
            (begin
                ;; Remove position listing
                (map-delete position-listings entity-owner)
                (ok "Position successfully withdrawn from the network.")
            )
            (err RESPONSE-ENTITY-MISSING)
        )
    )
)

;; ================= PUBLIC FUNCTIONS: INDIVIDUAL OPERATIONS =================

;; Create a new individual profile
(define-public (create-individual-profile 
    (identity (string-ascii 100))
    (capabilities (list 10 (string-ascii 50)))
    (location (string-ascii 100))
    (history (string-ascii 500)))
    (let
        (
            (entity-owner tx-sender)
            (existing-record (map-get? individual-records entity-owner))
        )
        ;; Verify profile doesn't exist
        (if (is-none existing-record)
            (begin
                ;; Validate mandatory information
                (if (or (is-eq identity "")
                        (is-eq location "")
                        (is-eq (len capabilities) u0)
                        (is-eq history ""))
                    (err RESPONSE-INVALID-HISTORY)
                    (begin
                        ;; Store individual profile
                        (map-set individual-records entity-owner
                            {
                                identity: identity,
                                capabilities: capabilities,
                                location: location,
                                history: history
                            }
                        )
                        (ok "Individual profile successfully created in the network.")
                    )
                )
            )
            (err RESPONSE-ALREADY-EXISTS)
        )
    )
)

;; Update existing individual profile
(define-public (update-individual-profile 
    (identity (string-ascii 100))
    (capabilities (list 10 (string-ascii 50)))
    (location (string-ascii 100))
    (history (string-ascii 500)))
    (let
        (
            (entity-owner tx-sender)
            (existing-record (map-get? individual-records entity-owner))
        )
        ;; Verify profile exists
        (if (is-some existing-record)
            (begin
                ;; Validate all required information
                (if (or (is-eq identity "")
                        (is-eq location "")
                        (is-eq (len capabilities) u0)
                        (is-eq history ""))
                    (err RESPONSE-INVALID-HISTORY)
                    (begin
                        ;; Update the profile
                        (map-set individual-records entity-owner
                            {
                                identity: identity,
                                capabilities: capabilities,
                                location: location,
                                history: history
                            }
                        )
                        (ok "Individual profile successfully updated in the registry.")
                    )
                )
            )
            (err RESPONSE-ENTITY-MISSING)
        )
    )
)

;; Remove individual profile from registry
(define-public (remove-individual-profile)
    (let
        (
            (entity-owner tx-sender)
            (existing-record (map-get? individual-records entity-owner))
        )
        ;; Verify profile existence
        (if (is-some existing-record)
            (begin
                ;; Delete profile from registry
                (map-delete individual-records entity-owner)
                (ok "Individual profile successfully removed from the network.")
            )
            (err RESPONSE-ENTITY-MISSING)
        )
    )
)

;; ================= READ-ONLY FUNCTIONS =================

;; Retrieve individual profile details
(define-read-only (get-individual-details (account-id principal))
    (match (map-get? individual-records account-id)
        profile-data (ok profile-data)
        RESPONSE-NOT-FOUND
    )
)

;; Retrieve organization details
(define-read-only (get-organization-details (account-id principal))
    (match (map-get? organization-registry account-id)
        org-data (ok org-data)
        RESPONSE-NOT-FOUND
    )
)

;; Retrieve position listing details
(define-read-only (get-position-details (listing-id principal))
    (match (map-get? position-listings listing-id)
        position-data (ok position-data)
        RESPONSE-NOT-FOUND
    )
)

