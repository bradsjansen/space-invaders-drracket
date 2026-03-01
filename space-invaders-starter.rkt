;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 3)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;; - empty
;; - cons (Invader ListOfInvader)
;; interp. an arbitrary number of Invaders

(define LOI0 empty)
(define LOI1 (cons I1 LOI0))
(define LOI2 (cons I2 LOI1))
(define LOI3 (cons I3 LOI2))

#;
(define (fn-for-loinvader loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first lod))
              (fn-for-loinvader (rest lod)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Invader ListOfInvader)
;;  - reference: (first loi) is Invader 
;;  - self-reference: (rest loi) is ListOfInvader

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;; - empty
;; - cons (Missile ListOfMissile)
;; interp. an arbitrary number of Missiles

(define LOM0 empty)
(define LOM1 (cons M1 LOM0))
(define LOM2 (cons M2 LOM1))
(define LOM3 (cons M3 LOM2))

#;
(define (fn-for-lomissile lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first lom) is Missile
;; - self-reference: (rest lom) is ListOfMissile

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; Functions:

;; Game -> Game
;; start game with initial state s (main G0)
;; no examples needed for main function

(define (main s)
  (big-bang s                       ;Game
            (on-tick update-game)   ;Game -> Game
            (to-draw render-game)   ;Game -> Image
            (on-key move-and-fire)  ;Game KeyEvent -> Game
            (stop-when landed?)))   ;Game -> Boolean


;; Game -> Game
;; update the current state of a Space Invaders game
(check-expect (update-game G0) (make-game
                                (update-invaders LOI0 LOM0)
                                (update-missiles LOM0 LOI0) 
                                (update-tank T0)))
(check-expect (update-game G2) (make-game
                                (update-invaders LOI1 LOM1)
                                (update-missiles LOM1 LOI1)
                                (update-tank T1)))
(check-expect (update-game G3) (make-game
                                (update-invaders (list I1 I2) (list M1 M2))
                                (update-missiles (list M1 M2) (list I1 I2))
                                (update-tank T1)))

;(define (update-game s) G0) ;stub

;<Template used from Game>
(define (update-game s)
  (make-game (update-invaders (game-invaders s) (game-missiles s))
             (update-missiles (game-missiles s) (game-invaders s))
             (update-tank (game-tank s))))

;; ListOfInvader ListOfMissile -> ListOfInvader
;; - move invaders along x,y position with time
;; - add invaders at a random x position at top of BACKGROUND
;; - remove invaders hit by missiles

(define (update-invaders loi lom)
  (move-invaders
   (add-invaders 
    (remove-invaders loi lom))))

;; ListOfInvader -> ListOfInvader
;; produce an arbitrary amount of invaders
(check-expect (move-invaders LOI0) empty)
(check-expect (move-invaders (cons (make-invader 150 100 12) empty))
                             (cons (make-invader (+ 150 12) (+ 100 INVADER-Y-SPEED) 12) empty))
(check-expect (move-invaders (cons (make-invader 150 510 10)
                                   (cons (make-invader 150 HEIGHT -10)
                                         (cons (make-invader 150 100 12) empty))))
              (cons (make-invader (+ 150 10) (+ 510 INVADER-Y-SPEED) 10)
                    (cons (make-invader (- 150 10) (+ HEIGHT INVADER-Y-SPEED) -10)
                          (cons (make-invader (+ 150 12) (+ 100 INVADER-Y-SPEED) 12) empty))))


;(define (move-invaders loi) empty) ;stub

;<Template from ListOfInvader>
(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader (first loi))
               (move-invaders (rest loi)))]))

;; Invader -> Invader
;; produce invader at x, y position and move along invader-dx
(check-expect (move-invader I1) (make-invader (+ 150 12) (+ 100 INVADER-Y-SPEED) 12))
(check-expect (move-invader I2) (make-invader (+ 150 -10) (+ HEIGHT INVADER-Y-SPEED) -10))

;(define (move-invader i) i) ;stub

;<Template from Invader>
(define (move-invader invader)
  (cond [(> (+ (invader-x invader) (invader-dx invader)) WIDTH) (make-invader
                                                                 WIDTH
                                                                 (+ (invader-y invader) INVADER-Y-SPEED)
                                                                 (- (invader-dx invader)))]
        [(< (+ (invader-x invader) (invader-dx invader)) 0) (make-invader
                                                             0
                                                             (+ (invader-y invader) INVADER-Y-SPEED)
                                                             (- (invader-dx invader)))]
        [else
         (make-invader (+ (invader-x invader) (invader-dx invader))
                       (+ (invader-y invader) INVADER-Y-SPEED)
                       (invader-dx invader))]))


;; ListOfInvader -> ListOfInvader
;; add invaders at random x position at top of BACKGROUND
;; using the INVADE-RATE constant

;(define (add-invaders loi) loi) ;stub

;<Template from ListOfInvader>
(define (add-invaders loi)
  (cond [(<= (random 5000) INVADE-RATE) (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi)]
        [else loi]))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; remove invaders hit by missiles
(check-expect (remove-invaders LOI0 LOM0) empty)
(check-expect (remove-invaders LOI1 LOM2) empty)
(check-expect (remove-invaders LOI1 LOM1) LOI1)

;(define (remove-invaders loi lom) loi) ;stub

;<Template from ListOfInvader with extra atomic parameter>
(define (remove-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (invader-hit? (first loi) lom)
             (remove-invaders (rest loi) lom)
             (cons (first loi) (remove-invaders (rest loi) lom)))]))

;; Invader ListOfMissile -> Boolean
;; produce true if Invader is hit by Missile
(check-expect (invader-hit? I1 LOM2) true)
(check-expect (invader-hit? I1 LOM1) false)

;(define (invader-hit? loi lom) true) ;stub

;<Template from ListOfInvader with extra atomic parameter>
(define (invader-hit? invader lom)
  (cond [(empty? lom) false]
        [else
         (if (in-range? invader (first lom))
             true
             (invader-hit? invader (rest lom)))]))

;; Invader Missile -> Boolean
;; produce true if Missile is within HIT-RANGE of Invader
(check-expect (in-range? I1 M1) false)
(check-expect (in-range? I1 M2) true)

;(define (in-range? invader m) true) ;stub

;<Template from Invader with extra atomic parameter>
(define (in-range? invader m)
  (and (<= (abs (- (missile-x m) (invader-x invader))) HIT-RANGE)
       (<= (abs (- (missile-y m) (invader-y invader))) HIT-RANGE)))

;; ListofMissile ListOfInvader -> ListOfMissile
;; produce Missile at missile-x position and progress upwards on BACKGROUND
;; remove missiles that hit invaders

(define (update-missiles lom loi)
  (move-missiles 
   (remove-missiles lom loi)))

;; ListOfMissile -> ListOfMissile
;; produce an abitarty amount of missiles progress upward by MISSILE-SPEED
(check-expect (move-missiles LOM0) empty)
(check-expect (move-missiles (cons (make-missile 150 105)
                                   (cons (make-missile 150 110)
                                         (cons (make-missile 150 300) empty))))
              (cons (make-missile 150 (- 105 MISSILE-SPEED))
                    (cons (make-missile 150 (- 110 MISSILE-SPEED))
                          (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty))))

;(define (move-missiles lom) lom) ;stub

;<Template from ListOfMissile>
(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (move-missile (first lom))
               (move-missiles (rest lom)))]))


;; Missile -> Missile
;; produce a missile at missile-x position and progress upward by MISSILE-SPEED
(check-expect (move-missile M1) (make-missile 150 (- 300 MISSILE-SPEED)))
(check-expect (move-missile M2) (make-missile 150 (- 110 MISSILE-SPEED)))

;(define (move-missile m) m) ;stub

;<Template from Missile>
(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissile ListOfInvader -> ListOfInvader
;; remove missiles that hit invaders
;; remove missiles that travel off-screen
(check-expect (remove-missiles LOM0 LOI0) empty)
(check-expect (remove-missiles LOM2 LOI1) LOM1)
(check-expect (remove-missiles LOM1 LOI1) LOM1)

;(define (remove-missiles lom loi) lom) ;stub

;<Template from ListOfInvader with extra atomic parameter>
(define (remove-missiles lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (or (missile-hit? (first lom) loi)
                 (missile-over-top? (first lom)))
             (remove-missiles (rest lom) loi)
             (cons (first lom) (remove-missiles (rest lom) loi)))]))

;; Missile ListOfInvader -> Boolean
;; produce true if Missile hits Invader
(check-expect (missile-hit? M2 LOI1) true)
(check-expect (missile-hit? M1 LOI1) false)

;(define (invader-hit? m loi) true) ;stub

;<Template from ListOfMissile with extra atomic parameter>
(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [else
         (if (in-range? (first loi) m)
             true
             (missile-hit? m (rest loi)))]))

;; Missile -> Boolean
;; produce true if Missile travels above BACKGROUND
(check-expect (missile-over-top? M1) false)
(check-expect (missile-over-top? (make-missile 150 -1)) true)

;(define (missile-over-top? m) true) ;stub

;<Template from Missile>
(define (missile-over-top? m)
  (< (missile-y m) 0))


;; Tank -> Tank
;; produce tank at location x and move left if dir -1 and right if dir 1
(check-expect (update-tank T1) (make-tank (+ 50 (* 1 TANK-SPEED)) 1))
(check-expect (update-tank T2) (make-tank (+ 50 (* -1 TANK-SPEED)) -1))

;(define (update-tank t) t) ;stub

;Template from Tank
(define (update-tank t)
  (cond [(> (+ (tank-x t) (tank-dir t)) WIDTH) (make-tank WIDTH (tank-dir t))]
        [(< (+ (tank-x t) (tank-dir t)) 0) (make-tank 0 (tank-dir t))]
        [else
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))


;; Game -> Image
;; render the current state of Game on BACKGROUND
(check-expect (render-game G3) (render-invaders (list I1 I2)
                                                (render-missiles (list M1 M2)
                                                                 (render-tank T1))))

;(define (render-game s) BACKGROUND) ;stub

(define (render-game s)
  (render-invaders (game-invaders s)
                   (render-missiles (game-missiles s)
                                    (render-tank (game-tank s)))))


;; ListOfInvader -> Image
;; render a given list of invaders at appropiate spot on BACKGROUND
(check-expect (render-invaders LOI0 BACKGROUND) BACKGROUND)
(check-expect (render-invaders LOI2 BACKGROUND) (place-image INVADER 150 HEIGHT 
                                                  (place-image INVADER 150 100 BACKGROUND)))


;(define (render-invaders loi) BACKGROUND)

;<Template from ListOfInvaders
(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (render-invader (first loi)
                         (render-invaders (rest loi) img))]))

;; Invader Image -> Image
;; render a given invader at appropriate spot on BACKGROUND
(check-expect (render-invader I1 BACKGROUND) (place-image INVADER 150 100 BACKGROUND))
(check-expect (render-invader I2 BACKGROUND) (place-image INVADER 150 HEIGHT BACKGROUND))

;(define (render-invader invader img) BACKGROUND) ;stub

;<Template from Invader with extra atomic parameter>
(define (render-invader invader img)
  (place-image INVADER (invader-x invader) (invader-y invader) img))


;; ListOfMissile -> Image
;; render a given list of missiles at appropriate spot on BACKGROUND
(check-expect (render-missiles LOM0 BACKGROUND) BACKGROUND)
(check-expect (render-missiles LOM3 BACKGROUND) (place-image MISSILE 150 105
                                                  (place-image MISSILE 150 110
                                                               (place-image MISSILE 150 300 BACKGROUND))))

;(define (render-missiles lom) BACKGROUND) ;stub

;<Template from ListOfMissile>
(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (render-missile (first lom)
                         (render-missiles (rest lom) img))]))

;; Missile Image -> Image
;; render a given Missile at appropriate spot on BACKGROUND
(check-expect (render-missile M1 BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))
(check-expect (render-missile M2 BACKGROUND) (place-image MISSILE 150 110 BACKGROUND))

;(define (render-missile m img) BACKGROUND) ;stub

;<Template from Missile>
(define (render-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))


;; Tank -> Image
;; render tank at appropriate x location on BACKGROUND and HEIGHT - TANK-HEIGHT/2
(check-expect (render-tank T0) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-tank t) BACKGROUND) ;stub

;Template from Tank
(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))


;; Tank KeyEvent -> Tank
;; On-Key move tank left with left arrow key and right with right arrow key
;; On-Key fire missiles from tank-x with " " key
(check-expect (move-and-fire (make-game empty empty T2) "right") (make-game empty empty (make-tank 50 1)))
(check-expect (move-and-fire (make-game empty empty T1) "left") (make-game empty empty (make-tank 50 -1)))
(check-expect (move-and-fire (make-game empty empty T2) " ")
              (make-game empty (cons (make-missile 50 (- (- HEIGHT TANK-HEIGHT/2) HIT-RANGE)) empty)
                                     (make-tank 50 -1)))

;(define (move-and-fire t ke) t) ;stub

(define (move-and-fire s ke)
  (cond [(key=? ke "right") (make-game (game-invaders s) (game-missiles s) (make-tank (tank-x (game-tank s)) 1))]
        [(key=? ke "left") (make-game (game-invaders s) (game-missiles s) (make-tank (tank-x (game-tank s)) -1))]
        [(key=? ke " ") (add-missile s)]
        [else s]))

;; Game -> Game
;; add missile to a ListOfMissiles in a Game, at tank-x position
(check-expect (add-missile G3) (make-game (game-invaders G3)
                                          (cons (make-missile (tank-x (game-tank G3))
                                                              (- (- HEIGHT TANK-HEIGHT/2) HIT-RANGE))
                                                (game-missiles G3))
                                                (game-tank G3)))

;(define (add-missile G3) G3) ;stub

;<Template used from Game>
(define (add-missile s)
  (make-game (game-invaders s)
             (cons (make-missile (tank-x (game-tank s)) (- (- HEIGHT TANK-HEIGHT/2) HIT-RANGE))
                   (game-missiles s))
             (game-tank s)))

   
;; Invader -> Boolean
;; stop Game if invader touches bottom of BACKGROUND
(check-expect (landed? G0) false)
(check-expect (landed? G2) false)
(check-expect (landed? G3) true)

;(define (landed? s) true) ;stub

;<Template used from Game>
(define (landed? s)
  (cond [(empty? (game-invaders s)) false]
        [else
         (if (>= (invader-y (first (game-invaders s))) HEIGHT)
             true
             (landed? (make-game (rest (game-invaders s))
                                 (game-missiles s)
                                 (game-tank s))))]))     