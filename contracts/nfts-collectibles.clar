(define-non-fungible-token raffle-nft uint)

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_RAFFLE_NOT_ACTIVE (err u101))
(define-constant ERR_ALREADY_ENTERED (err u102))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u103))
(define-constant ERR_RAFFLE_ALREADY_DRAWN (err u104))
(define-constant ERR_NO_PARTICIPANTS (err u105))
(define-constant ERR_MINT_FAILED (err u106))
(define-constant ERR_TRANSFER_FAILED (err u107))
(define-constant ERR_RAFFLE_NOT_FOUND (err u108))
(define-constant ERR_REFUND_FAILED (err u109))
(define-constant ERR_RAFFLE_STILL_ACTIVE (err u110))
(define-constant ERR_ALREADY_REFUNDED (err u111))

(define-data-var next-raffle-id uint u1)
(define-data-var next-nft-id uint u1)

(define-map raffles
  uint
  {
    creator: principal,
    entry-fee: uint,
    max-participants: uint,
    current-participants: uint,
    is-active: bool,
    is-drawn: bool,
    winner: (optional principal),
    nft-id: (optional uint),
    stacks-block-height-created: uint
  }
)

(define-map raffle-participants
  { raffle-id: uint, participant: principal }
  { entry-block: uint, entry-index: uint }
)

(define-map participant-list
  { raffle-id: uint, index: uint }
  principal
)

(define-map user-raffle-entries
  { user: principal, raffle-id: uint }
  bool
)

(define-map refund-claims
  { user: principal, raffle-id: uint }
  bool
)

(define-read-only (get-raffle (raffle-id uint))
  (map-get? raffles raffle-id)
)

(define-read-only (get-next-raffle-id)
  (var-get next-raffle-id)
)

(define-read-only (get-next-nft-id)
  (var-get next-nft-id)
)

(define-read-only (has-user-entered (raffle-id uint) (user principal))
  (default-to false (map-get? user-raffle-entries { user: user, raffle-id: raffle-id }))
)

(define-read-only (get-participant-at-index (raffle-id uint) (index uint))
  (map-get? participant-list { raffle-id: raffle-id, index: index })
)

(define-read-only (get-nft-owner (nft-id uint))
  (nft-get-owner? raffle-nft nft-id)
)

(define-read-only (has-claimed-refund (raffle-id uint) (user principal))
  (default-to false (map-get? refund-claims { user: user, raffle-id: raffle-id }))
)

(define-read-only (is-refund-eligible (raffle-id uint))
  (match (map-get? raffles raffle-id)
    raffle-data (and
      (not (get is-active raffle-data))
      (not (get is-drawn raffle-data))
      (> (get current-participants raffle-data) u0))
    false)
)

(define-public (create-raffle (entry-fee uint) (max-participants uint))
  (let
    (
      (raffle-id (var-get next-raffle-id))
    )
    (asserts! (> max-participants u0) ERR_NOT_AUTHORIZED)
    (asserts! (> entry-fee u0) ERR_NOT_AUTHORIZED)
    (map-set raffles raffle-id
      {
        creator: tx-sender,
        entry-fee: entry-fee,
        max-participants: max-participants,
        current-participants: u0,
        is-active: true,
        is-drawn: false,
        winner: none,
        nft-id: none,
        stacks-block-height-created: stacks-block-height
      }
    )
    (var-set next-raffle-id (+ raffle-id u1))
    (ok raffle-id)
  )
)

(define-public (enter-raffle (raffle-id uint))
  (let
    (
      (raffle-data (unwrap! (map-get? raffles raffle-id) ERR_RAFFLE_NOT_FOUND))
      (current-count (get current-participants raffle-data))
    )
    (asserts! (get is-active raffle-data) ERR_RAFFLE_NOT_ACTIVE)
    (asserts! (not (get is-drawn raffle-data)) ERR_RAFFLE_ALREADY_DRAWN)
    (asserts! (< current-count (get max-participants raffle-data)) ERR_RAFFLE_NOT_ACTIVE)
    (asserts! (not (has-user-entered raffle-id tx-sender)) ERR_ALREADY_ENTERED)
    
    (try! (stx-transfer? (get entry-fee raffle-data) tx-sender (get creator raffle-data)))
    
    (map-set raffle-participants
      { raffle-id: raffle-id, participant: tx-sender }
      { entry-block: stacks-block-height, entry-index: current-count }
    )
    
    (map-set participant-list
      { raffle-id: raffle-id, index: current-count }
      tx-sender
    )
    
    (map-set user-raffle-entries
      { user: tx-sender, raffle-id: raffle-id }
      true
    )
    
    (map-set raffles raffle-id
      (merge raffle-data { current-participants: (+ current-count u1) })
    )
    
    (ok true)
  )
)

(define-public (draw-winner (raffle-id uint))
  (let
    (
      (raffle-data (unwrap! (map-get? raffles raffle-id) ERR_RAFFLE_NOT_FOUND))
      (participant-count (get current-participants raffle-data))
      (random-seed (+ stacks-block-height (get stacks-block-height-created raffle-data)))
      (winner-index (mod random-seed participant-count))
      (winner (unwrap! (get-participant-at-index raffle-id winner-index) ERR_NO_PARTICIPANTS))
      (nft-id (var-get next-nft-id))
    )
    (asserts! (or (is-eq tx-sender (get creator raffle-data)) (is-eq tx-sender CONTRACT_OWNER)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active raffle-data) ERR_RAFFLE_NOT_ACTIVE)
    (asserts! (not (get is-drawn raffle-data)) ERR_RAFFLE_ALREADY_DRAWN)
    (asserts! (> participant-count u0) ERR_NO_PARTICIPANTS)
    
    (try! (nft-mint? raffle-nft nft-id winner))
    
    (map-set raffles raffle-id
      (merge raffle-data 
        { 
          is-drawn: true,
          is-active: false,
          winner: (some winner),
          nft-id: (some nft-id)
        }
      )
    )
    
    (var-set next-nft-id (+ nft-id u1))
    (ok { winner: winner, nft-id: nft-id })
  )
)

(define-public (cancel-raffle (raffle-id uint))
  (let
    (
      (raffle-data (unwrap! (map-get? raffles raffle-id) ERR_RAFFLE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator raffle-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active raffle-data) ERR_RAFFLE_NOT_ACTIVE)
    (asserts! (not (get is-drawn raffle-data)) ERR_RAFFLE_ALREADY_DRAWN)
    
    (map-set raffles raffle-id
      (merge raffle-data { is-active: false })
    )
    (ok true)
  )
)

(define-public (transfer-nft (nft-id uint) (recipient principal))
  (let
    (
      (current-owner (unwrap! (nft-get-owner? raffle-nft nft-id) ERR_TRANSFER_FAILED))
    )
    (asserts! (is-eq tx-sender current-owner) ERR_NOT_AUTHORIZED)
    (try! (nft-transfer? raffle-nft nft-id tx-sender recipient))
    (ok true)
  )
)

(define-public (claim-refund (raffle-id uint))
  (let
    (
      (raffle-data (unwrap! (map-get? raffles raffle-id) ERR_RAFFLE_NOT_FOUND))
      (entry-fee (get entry-fee raffle-data))
      (creator (get creator raffle-data))
    )
    (asserts! (has-user-entered raffle-id tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-refund-eligible raffle-id) ERR_RAFFLE_STILL_ACTIVE)
    (asserts! (not (has-claimed-refund raffle-id tx-sender)) ERR_ALREADY_REFUNDED)
    
    (try! (as-contract (stx-transfer? entry-fee creator tx-sender)))
    
    (map-set refund-claims
      { user: tx-sender, raffle-id: raffle-id }
      true
    )
    
    (ok entry-fee)
  )
)

(define-read-only (get-raffle-stats (raffle-id uint))
  (match (map-get? raffles raffle-id)
    raffle-data (ok {
      participants: (get current-participants raffle-data),
      max-participants: (get max-participants raffle-data),
      entry-fee: (get entry-fee raffle-data),
      is-active: (get is-active raffle-data),
      is-drawn: (get is-drawn raffle-data),
      winner: (get winner raffle-data),
      nft-id: (get nft-id raffle-data)
    })
    ERR_RAFFLE_NOT_FOUND
  )
)

(define-read-only (get-user-raffles-entered (user principal) (raffle-ids (list 50 uint)))
  (map has-user-entered-helper raffle-ids)
)

(define-private (has-user-entered-helper (raffle-id uint))
  {
    raffle-id: raffle-id,
    entered: (has-user-entered raffle-id tx-sender)
  }
)