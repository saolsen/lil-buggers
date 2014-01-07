#lang racket
#| 

Working my way through realm of racket for a bit then making a game called
lil buggers which is going to be real fun.
Grublies

|#

(require racket/set
         2htdp/universe
         2htdp/image
         2htdp/planetcute
         "noise.rkt")

;; PRECEDURAL GENERATION STUFF
;; ===========================

;; Seed, not sure how to use it yet.
(define SEED "someshit")

;; Takes the perlin noise value and returns
(define (terrain-value x)
  ;;  .75 - 1.0  -> forest
  ;;  .01 -  .74 -> grass
  ;; -.75 - 0.0  -> sand
  ;; -1.0 - -.74 -> water
  (cond [(< x -0.75) 'water]
        [(< x 0) 'sand]
        [(< x .75) 'grass]
        [else 'forest]))

(define (terrain-color t)
  (cond [(equal? t 'water)  "blue"]
        [(equal? t 'sand)   "tan"]
        [(equal? t 'grass)  "green"]
        [(equal? t 'forest) "darkgreen"]))

(define (terrain-tile t)
  (cond [(equal? t 'water)  water-block]
        [(equal? t 'sand)   dirt-block]
        [(equal? t 'grass)  grass-block]
        [(equal? t 'forest) stone-block]))

(define (noise-fn x y #:zoom [zoom 10])
  (simplex (/ x zoom) (/ y zoom)))

(define (get-terrain-type x y)
  (terrain-value (noise-fn x y)))

;; GAMEPLAY STUFF
;; ==============

(define SCREEN-WIDTH ;1024
  1366)
(define SCREEN-HEIGHT ;600
  768)
(define TILE-SCALE-X 100)
(define TILE-SCALE-Y 80)
(define TILE-SCALE-Z -40)
(define DRAW-HEIGHT (add1 (ceiling (/ SCREEN-WIDTH TILE-SCALE-Y))))
(define DRAW-WIDTH (add1 (ceiling (/ SCREEN-WIDTH TILE-SCALE-X))))

(define PLAYER-SPEED 1/5)
(define TICKS-PER-SECOND 60)

;; GameState
(struct gamestate (entities keysdown terrain) #:transparent)

;; Entities
(struct entity (id components) #:transparent)

;; Components
(struct position (val) #:transparent)
(struct velocity (val) #:transparent)
(struct icon (val) #:transparent)
(struct player () #:transparent)
(struct bugger () #:transparent)

(define (is-component-type t c)
    (let ([checker
           (cond
             [(equal? t position) position?]
             [(equal? t velocity) velocity?]
             [(equal? t icon) icon?]
             [(equal? t player) player?]
             [(equal? t bugger) bugger?])])
      (checker c)))

(define (get-with-components entities comp-types)
  (define (has-component? entity comp-type)
    (ormap
     (λ (component) (is-component-type comp-type component))
     (entity-components entity)))
  (filter
   (λ (entity)
     (andmap (curry has-component? entity) comp-types))
   entities))

;; Get a component from an entity
(define (get-component ent comp-type)
  (let ([comp-list (filter
                    (curry is-component-type comp-type)
                    (entity-components ent))])
    (if (empty? comp-list)
        #f
        (first comp-list))))

;; Returns the player entity
(define (get-player ents)
  (first (get-with-components ents (list player))))

;; Draw
(define (compute-screen-position center pos)
  (let ([x-offset (- (first center) (/ SCREEN-WIDTH 2))]
        [y-offset (- (second center) (/ SCREEN-HEIGHT 2))])
    (list (- (first pos) x-offset)
          (- (second pos) y-offset))))

(define (game-space->screen-space pos)
  (list (* TILE-SCALE-X (first pos))
        (+ (* TILE-SCALE-Y (second pos))
           (* TILE-SCALE-Z (third pos)))))

;; This transformation isn't 1-1 because it's 2 dimentions to 3. This could end up
;; being an issue when trying to click something that's on various z levels but can
;; deal with that when I get to it. This function currently assumes everything is at
;; z = 0, will need more logic for mapping mouseclick to entity for example.
(define (screen-space->game-space loc)
  (list (/ (first loc) 100)
        (/ (second loc) 80)
        0))

(define (draw-terrain terrain center-screen)
  (define center-x (first center-screen))
  (define center-y (second center-screen))
  (for*/fold ([canvas (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)])
      ([y (range (floor (- center-y (/ DRAW-HEIGHT 2)))
                 (ceiling (+ center-y (/ DRAW-HEIGHT 2))))]
       [x (range (floor (- center-x (/ DRAW-WIDTH 2)))
                 (ceiling  (+ center-x (/ DRAW-WIDTH 2))))])
    (let ([draw-position (compute-screen-position
                          (game-space->screen-space center-screen)
                          (game-space->screen-space (list x y 0)))]
          [t (hash-ref terrain (cons x y) #f)])
      (if t
          (place-image (terrain-tile t)
                       (first draw-position)
                       (second draw-position)
                       canvas)
        canvas))))

(define (draw-icons ents center-screen canvas)
  (let ([ents-with-icons (get-with-components ents (list icon position))])
    (foldl (λ (e screen)
              (let* ([img (icon-val (get-component e icon))]
                     [position (position-val (get-component e position))]
                     [draw-at
                      (compute-screen-position
                       (game-space->screen-space center-screen)
                       (game-space->screen-space position))]                    
                     [x (first draw-at)]
                     [y (second draw-at)])
                (place-image img x y screen)))
           canvas
           ents-with-icons)))

(define (render-game state)
  (let* ([ents (gamestate-entities state)]
         [terrain (gamestate-terrain state)]
         [player (get-player ents)]
         [player-position (position-val (get-component player position))]
         [canvas (draw-terrain terrain player-position)])
    (draw-icons ents player-position canvas)))

;; Update
;; ======

;; Takes the list of entities, replaces the component of comp-type
;; on the entity with id id with new-component.
;; Returns a new list of entities.
;; This can be used to swap non mutable components but with mutable ones
;; it's pretty overkill.
(define (swap-component ents ent-id comp-type new-comp)
  (let* ([ent (first (filter (λ (e) (equal? (entity-id e) ent-id)) ents))]
         [others (filter (λ (e) (not (equal? (entity-id e) ent-id))) ents)]
         [comps (filter
                 (λ (c) (not (is-component-type comp-type c)))
                 (entity-components ent))]
         [new-ent (struct-copy entity ent
                               [components (cons new-comp comps)])])
    (cons new-ent others)))

(define RENDER-DISTANCE 10)

;; Need to generate terrain before the player gets to it.
(define (generate-terrain w)
  (define current-terrain (gamestate-terrain w))
  (define player (get-player (gamestate-entities w)))
  (define player-pos (position-val (get-component player position)))
  (define player-pos-x (first player-pos))
  (define player-pos-y (second player-pos))
  (define new-terrain
    (for*/fold ([terrain current-terrain])
               ([x (range (- (floor player-pos-x) RENDER-DISTANCE)
                          (+ (floor player-pos-x) RENDER-DISTANCE))]
                [y (range (- (floor player-pos-y) RENDER-DISTANCE)
                          (+ (floor player-pos-y) RENDER-DISTANCE))]
                #:when (not (hash-has-key? terrain (cons x y))))
      (hash-set terrain (cons x y) (get-terrain-type x y))))
  (struct-copy gamestate w [terrain new-terrain]))

(define (get-player-velocity keys-down)
  (let ([apply-directional-velociy
         (λ (k v)
           (cond
             [(key=? k "w") (map + v '(0 -1 0))]
             [(key=? k "a") (map + v '(-1 0 0))]
             [(key=? k "s") (map + v '(0 1 0))]
             [(key=? k "d") (map + v '(1 0 0))]
             [else v]))])
    (map (curry * PLAYER-SPEED)
         (foldl apply-directional-velociy '(0 0 0) keys-down))))

;; System
;; Comps: velocity player
(define (set-player-velocity w)
  (let* ([ks (gamestate-keysdown w)]
         [v (get-player-velocity ks)]
         [ents (gamestate-entities w)]
         [player (get-player ents)]
         [players-current-velocity (get-component player velocity)])
    (struct-copy gamestate w
                 [entities
                  (swap-component ents (entity-id player) velocity (velocity v))])))

;; System
;; Comps: velocity position
(define (apply-velocity-to-position w)
  (define starting-ents (gamestate-entities w))
  (define new-ents
    (for/fold ([ents starting-ents])
              ([e starting-ents])
      (define v (get-component e velocity))
      (define p (get-component e position))
      (define new-p (and v p (map + (velocity-val v) (position-val p))))
      (if new-p
          (swap-component ents (entity-id e) position (position new-p))
          ents)))
  (struct-copy gamestate w [entities new-ents]))


;; System
;; Comps: bugger position
(define (test-bugger-ai w)
  ;; (let* ([ents (gamestate-entities w)]
  ;;        [buggers (get-with-components (gamestate-entities w)
  ;;                                      (list bugger position velocity))])
  ;;   (for ([e buggers])
  ;;     (set-velocity-val! (get-component e velocity) (list (/ (random) 100)
  ;;                                                         (/ (random) 100)
  ;;                                                         0.0))))
    w)

(define (update-game w)
  ((compose apply-velocity-to-position
            set-player-velocity
            test-bugger-ai
            generate-terrain) w))

;; KeyPresses
(define (keydown w a-key)
  (let ([keys (gamestate-keysdown w)])
    (if (not (member a-key keys))
        (struct-copy gamestate w [keysdown (cons a-key keys)])
        w)))

(define (keyup w a-key)
  (let ([keys (gamestate-keysdown w)])
    (struct-copy gamestate w
                 [keysdown (filter (λ (k) (not (key=? a-key k))) keys)])))


;; Graphics
(define (make-shitty-tree-icon)
  (overlay/align/offset
   "middle" "top"
   (above (triangle 50 "solid" "darkgreen")
          (triangle 50 "solid" "darkgreen")
          (triangle 50 "solid" "darkgreen"))
   0 10
   (rectangle 10 150 "solid" "brown")))

;; Initial scene.
(define test-scene
   (list
    (entity "rock"
            (list (position '(1 1 1))
                  (icon stone-block)))
    (entity "rock"
            (list (position '(2 1 1))
                  (icon stone-block)))
    (entity "rock"
            (list (position '(3 1 1))
                  (icon stone-block)))
    (entity "rock"
            (list (position '(2 1 2))
                  (icon stone-block)))
    (entity "bugger"
            (list (position '(1 5 0))
                  (velocity '(0 0 0))
                  (bugger)
                  (icon enemy-bug)))
    (entity "player"
            (list (player)
                  (position '(5 5 0))
                  (velocity '(0 0 0))
                  (icon character-horn-girl)))
    (entity "tree1"
            (list (position '(900 502 0))
                  (icon (make-shitty-tree-icon))))
    (entity "tree2"
            (list (position '(807 40 0))
                  (icon (make-shitty-tree-icon))))
    (entity "tree3"
            (list (position '(225 120 0))
                  (icon (make-shitty-tree-icon)))))
    )

(define (start-scene)
  (big-bang (gamestate test-scene
                       '()
                       (make-immutable-hash))
            (on-tick update-game (/ 1 TICKS-PER-SECOND))
            (on-key keydown)
            (on-release keyup)
            (to-draw render-game)))

(define (test) (gamestate-entities (start-scene)))

(gamestate-entities (start-scene))

