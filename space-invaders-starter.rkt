;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; for testing:
(define BLANK (square 0 "solid" "white"))
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define INVADER-MAX-SPEED 8) 
(define TANK-SPEED 2)
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

(define-struct game (invaders missiles tank counter))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles, tank position, and the current time of game

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))
       counter))



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
(define I2 (make-invader 300 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0 0))
(define G1 (make-game empty empty T1 1))
(define G2 (make-game (list I1) (list M1) T1 20))
(define G3 (make-game (list I1 I2) (list M1 M2) T1 3))

;; Functions:

;; Game -> Game
;; start the world with ...
;; 
(define (main ws)
  (big-bang ws                   ; Game
    (on-tick   tock)     ; Game -> Game
    (to-draw   render)   ; Game -> Image         
    (on-key    handle-key)
    (stop-when end-game? game-over)))    ; Game KeyEvent -> Game


;; Game -> Image
;; render the game on screen
(check-expect (render G0)
              (place-images (list TANK)
                            (list (make-posn (tank-x (game-tank G0)) (- HEIGHT TANK-HEIGHT/2)))
                            BACKGROUND))
(check-expect (render G1)
              (place-images (list TANK)
                            (list (make-posn (tank-x (game-tank G1)) (- HEIGHT TANK-HEIGHT/2)))
                            BACKGROUND))
(check-expect (render G2)
              (place-images (list INVADER
                                  MISSILE
                                  TANK)
                            (list (make-posn (invader-x (list-ref (game-invaders G2) 0)) (invader-y (list-ref (game-invaders G2) 0)))
                                  
                                  (make-posn (missile-x (list-ref (game-missiles G2) 0)) (missile-y (list-ref (game-missiles G2) 0)))
                                  
                                  (make-posn (tank-x (game-tank G2)) (- HEIGHT TANK-HEIGHT/2)))
                            BACKGROUND))
(check-expect (render G3)
              (place-images (list INVADER
                                  INVADER
                                  MISSILE
                                  MISSILE
                                  TANK)
                            (list (make-posn (invader-x (list-ref (game-invaders G3) 0)) (invader-y (list-ref (game-invaders G3) 0)))
                                  (make-posn (invader-x (list-ref (game-invaders G3) 1)) (invader-y (list-ref (game-invaders G3) 1)))
                                  (make-posn (missile-x (list-ref (game-missiles G3) 0)) (missile-y (list-ref (game-missiles G3) 0)))
                                  (make-posn (missile-x (list-ref (game-missiles G3) 1)) (missile-y (list-ref (game-missiles G3) 1))) 
                                  (make-posn (tank-x (game-tank G3)) (- HEIGHT TANK-HEIGHT/2)))
                            BACKGROUND))

; (define (render g) (square 0 "solid" "white"))  ;stub
(define (render s)
  (place-images (append (produce-images INVADER (game-invaders s))
                        (produce-images MISSILE (game-missiles s))
                        (cons TANK empty))
                (append (invaders-posn (game-invaders s))
                        (missiles-posn (game-missiles s))
                        (cons (make-posn (tank-x (game-tank s)) (- HEIGHT TANK-HEIGHT/2)) empty))
                BACKGROUND))

;; Lists -> ListOfImage
;; produce a list of image IMG n number of times,
;;                     where n = number of invaders/missiles in the list
(check-expect (produce-images INVADER empty) empty)
(check-expect (produce-images INVADER (list I1 I2))
              (list INVADER INVADER))
(check-expect (produce-images MISSILE (list M1 M2))
              (list MISSILE MISSILE))
; (define (produce-images img li) BLANK)  ;stub
(define (produce-images img li)
  (cond [(empty? li) empty]
        [else
         (cons img
               (produce-images img (rest li)))]))


;; ListOfInvaders -> ListOfPosnOfInvader
;; extract a list of (make-posn invader-x invader-y) from the given list of invaders
(check-expect (invaders-posn empty) empty)
(check-expect (invaders-posn (list I1 I2))
              (list (make-posn (invader-x I1) (invader-y I1))
                    (make-posn (invader-x I2) (invader-y I2))))
; (define (invaders-posn loin) (cons (make-posn 0 0) empty))  ;stub
(define (invaders-posn loin)
  (cond [(empty? loin) empty]
        [else
         (cons (make-posn (invader-x (first loin)) (invader-y (first loin)))
               (invaders-posn (rest loin)))]))


;; ListOfMissiles -> ListOfPosnOfMissiles
;; extract a list of (make-posn missiles-x missiles-y) from the given list of missiles
(check-expect (missiles-posn empty) empty)
(check-expect (missiles-posn (list M1 M2))
              (list (make-posn (missile-x M1) (missile-y M1))
                    (make-posn (missile-x M2) (missile-y M2))))
; (define (missiles-posn lom) (cons (make-posn 3 5) empty))   ;STUB
(define (missiles-posn lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-posn (missile-x (first lom)) (missile-y (first lom)))
               (missiles-posn (rest lom)))]))

 


;; Game -> Game
;; move the invaders downward by INVADER-X/Y-SPEED
;; move the missile forward by MISSILE-SPEED
;; move the tank TANK-SPEED pixels per clock tick left if dir -1, right if dir 1
(check-random (tock G0)
              (make-game (move-invaders (if (equal? (modulo (game-counter G0) (+ 10 (random 40))) 0)
                                            (spawn-invader (handle-invader-collision (game-invaders G0)
                                                                                     (game-missiles G0)))
                                            (handle-invader-collision (game-invaders G0)
                                                                      (game-missiles G0))))
                         empty
                         (make-tank (+ (tank-x (game-tank G0)) TANK-SPEED) (tank-dir (game-tank G0)))
                         (+ (game-counter G0) 1)))
(check-random (tock G2)
              (make-game (move-invaders (if (equal? (modulo (game-counter G2) (+ 10 (random 40))) 0)
                                            (spawn-invader (handle-invader-collision (game-invaders G2)
                                                                                     (game-missiles G2)))
                                            (handle-invader-collision (game-invaders G2) (game-missiles G2))))
                         (filter-missiles (move-missiles (handle-missile-collision (game-missiles G2)
                                                                                   (game-invaders G2))))
                         (move-tank (game-tank G2))
                         (+ (game-counter G2) 1)))
; (define (tock g) g)         ;stub
(define (tock g)
  (make-game (move-invaders (if (equal? (modulo (game-counter g) (+ 10 (random 40))) 0)
                                (spawn-invader (handle-invader-collision (game-invaders g)
                                                                         (game-missiles g)))
                                (handle-invader-collision (game-invaders g)
                                                          (game-missiles g))))
             (filter-missiles (move-missiles (handle-missile-collision (game-missiles g)
                                                                       (game-invaders g))))
             (move-tank (game-tank g))
             (+ (game-counter g) 1)))

;; ListOfInvaders -> ListOfInvaders
;; add new invader at random location
(check-random (spawn-invader empty)
              (list (make-invader (random WIDTH) 0 (random INVADER-MAX-SPEED))))
(check-random (spawn-invader (list (make-invader 100 200 -5) (make-invader 50 195 -5)))
              (list (make-invader (random WIDTH) 0 (random INVADER-MAX-SPEED))
                    (make-invader 100 200 -5)
                    (make-invader 50 195 -5)))
; (define (spawn-invader loin) loin)  ;stub
(define (spawn-invader loin)
  (cons (make-invader (random WIDTH) 0 (random INVADER-MAX-SPEED)) loin))


;; ListOfInvaders -> ListOfInvaders
;; add INVADER-SPEED to (invader-y invader) and add (invader-dx invader) to (invader-x invader)
;; move the invader downwards at 45 degree angle. if invader touches the edge of window. change invader direction
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (list I1 I2))
              (list (make-invader (+ (invader-x I1) (invader-dx I1))
                                  (+ (invader-y I1) INVADER-Y-SPEED)
                                  (invader-dx I1))
                    (make-invader (+ (invader-x I2) (invader-dx I2))
                                  (+ (invader-y I2) INVADER-Y-SPEED)
                                  (invader-dx I2))))
; (define (move-invaders loin) loin)   ;stub
(define (move-invaders loin)
  (cond [(empty? loin) empty]
        [else
         (cons (bounce-invader (make-invader (+ (invader-x (first loin)) (invader-dx (first loin)))
                                             (+ (invader-y (first loin)) INVADER-Y-SPEED)
                                             (invader-dx (first loin))))
               (move-invaders (rest loin)))]))

;; Invader -> Invader
;; bounce the invader that touch the wall of window. i.e. invert their dx
(check-expect (bounce-invader (make-invader 150 200 -10))
              (make-invader 150 200 -10))
(check-expect (bounce-invader (make-invader 301 300 10))
              (make-invader 300 300 -10))
(check-expect (bounce-invader (make-invader -2 400 -5))
              (make-invader 0 400 5))
; (define (bounce-invader loin) loin)  ;stub `
(define (bounce-invader invader)
  (cond [(<= (invader-x invader) 0) (make-invader 0 (invader-y invader) (- (invader-dx invader)))]
        [(>= (invader-x invader) WIDTH) (make-invader 300 (invader-y invader) (- (invader-dx invader)))]
        (else invader)))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; remove the invader if any missile comes within HIT-RANGE pixels of the invader
(check-expect (handle-invader-collision empty (list (make-missile 50 100)))        ; no invaders   
              empty)
(check-expect (handle-invader-collision (list (make-invader 200 100 5) (make-invader 100 200 10))       ;missile hits first invader
                                        (list (make-missile 200 97)))
              (list (make-invader 100 200 10)))
(check-expect (handle-invader-collision (list (make-invader 200 100 5) (make-invader 100 200 10))       ;both missile hits both invader
                                        (list (make-missile 200 93) (make-missile 100 198)))
              empty)
(check-expect (handle-invader-collision (list (make-invader 200 100 4) (make-invader 100 200 10))       ;both missile doesn't hit any invader
                                        (list (make-missile 300 300) (make-missile 250 250)))
              (list (make-invader 200 100 4) (make-invader 100 200 10)))
; (define (handle-invader-collision loin lom) loin)  ;stub
(define (handle-invader-collision loin lom)
  (cond [(empty? loin) empty]
        [else
         (if (invader-hit? (first loin) lom)
             (handle-invader-collision (rest loin) lom)
             (cons (first loin) (handle-invader-collision (rest loin) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; produce true if invader collided with any missile else false
(check-expect (invader-hit? (make-invader 200 100 5)
                            (list (make-missile 200 93) (make-missile 100 198)))
              true)   ; hit
(check-expect (invader-hit? (make-invader 300 200 3)
                            (list (make-missile 200 93) (make-missile 100 20)))
              false)   ; not hit
; (define (invader-hit? i lom) false) ;stub
(define (invader-hit? i lom)
  (cond [(empty? lom) false]
        [else
         (if (and (<= (abs (- (invader-x i) (missile-x (first lom)))) HIT-RANGE)
                  (<= (abs (- (invader-y i) (missile-y (first lom)))) HIT-RANGE))
             true
             (invader-hit? i (rest lom)))]))

;; ListOfMissile ListOfInvaders -> ListOfMissiles
;; remove the missile that comes within HIT-RANGE pixels of the invader
(check-expect (handle-missile-collision (list (make-missile 50 100)) empty)      ; no invaders   
              (list (make-missile 50 100)))
(check-expect (handle-missile-collision (list (make-missile 200 97))
                                        (list (make-invader 200 100 5) (make-invader 100 200 10)))       ;missile hits first invader
              empty)
(check-expect (handle-missile-collision (list (make-missile 200 93) (make-missile 100 198))
                                        (list (make-invader 200 100 5) (make-invader 100 200 10)))       ;both missile hits both invader                               
              empty)
(check-expect (handle-missile-collision (list (make-missile 300 300) (make-missile 250 250))
                                        (list (make-invader 200 100 4) (make-invader 100 200 10)))       ;both missile doesn't hit any invader                                        
              (list (make-missile 300 300) (make-missile 250 250)))
; (define (handle-missile-collision lom loin) lom)     ;stub
(define (handle-missile-collision lom loin)
  (cond [(empty? lom) empty]
        [else
         (if (missile-hit? (first lom) loin)
             (handle-missile-collision (rest lom) loin)
             (cons (first lom) (handle-missile-collision (rest lom) loin)))]))

;; Missile ListOfInvaders -> Boolean
;; produce true if missile comes withing HIT-RANGE of invader else false
(check-expect (missile-hit? (make-missile 50 100)
                            (list (make-invader 50 100 -5) (make-invader 200 100 5) (make-invader 100 200 10)))  ;missile hit
              true)
(check-expect (missile-hit? (make-missile 50 100)
                            (list (make-invader 30 200 5) (make-invader 100 200 10)))          ;missile not hit
              false)
; (define (missile-hit? m loin) false)  ;stub
(define (missile-hit? m loin)
  (cond [(empty? loin) false]
        [else
         (if (and (<= (abs (- (missile-x m) (invader-x (first loin)))) HIT-RANGE)
                  (<= (abs (- (missile-y m) (invader-y (first loin)))) HIT-RANGE))
             true
             (missile-hit? m (rest loin)))]))


;; ListOfMissiles -> ListOfMissiles
;; substract MISSILE-SPEED FROM (missile-y missile) until it becomes zero.
;; when it becomes zero pop it off the list of missiles
(check-expect (move-missiles empty) empty)
(check-expect (move-missiles (list M1 M2))
              (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))
                    (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED))))
; (define (move-missiles lom) lom)     ;stub
(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
               (move-missiles (rest lom)))]))


;; ListOfMissiles -> ListOfMissile
;; remove the missile that has y coordinate < 0 i.e. missile has gone out of window
(check-expect (filter-missiles empty) empty)
(check-expect (filter-missiles (list (make-missile 150 200)
                                     (make-missile 150 0)
                                     (make-missile 150 -10)))
              (list (make-missile 150 200)
                    (make-missile 150 0)))
; (define (filter-missiles lom) lom)  ;stub
(define (filter-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (< (missile-y (first lom)) 0)
             (filter-missiles (rest lom))
             (cons (first lom) (filter-missiles (rest lom))))]))


;; Tank -> Tank
;; add TANK-SPEED to (tank-x tank) until it reaches either of the edge
;; reverse the direction of tank once it reaches edge
(check-expect (move-tank (make-tank 150 1))
              (make-tank (+ 150 TANK-SPEED) 1))   ; tank in middle of window going right
(check-expect (move-tank (make-tank 150 -1))      ;tank in middle of window going left
              (make-tank (+ 150 (* TANK-SPEED -1)) -1))
(check-expect (move-tank (make-tank 305 1))            ; tank crossed window border
              (make-tank 300 -1))
(check-expect (move-tank (make-tank -2 -1))
              (make-tank 0 1))
; (define (move-tank t) t)       ;stub
(define (move-tank t)
  (cond [(< (tank-x t) 0) (make-tank 0 (- (tank-dir t)))]
        [(> (tank-x t) WIDTH) (make-tank WIDTH (- (tank-dir t)))]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))



;; Game KeyEvent -> Game
;; add a new missile at x pos of tank when space pressed
;; change direction of tank based on which key pressed:
;;                  - left arrow key: dir = -1
;;                  - right arrow key: dir = 1
; (define (handle-key g ke) g)   ;stub
(define (handle-key g ke)
  (cond [(key=? ke "left") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1) (game-counter g))]
        [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1) (game-counter g))]
        [(key=? ke " ") (make-game (game-invaders g) (add-missile (game-missiles g) (tank-x (game-tank g))) (game-tank g) (game-counter g))]
        [else 
         g]))

;; ListOfMissile Number[0, WIDTH]-> ListOfMissile
;; add a new missile into lom with x and y (HIEGHT - TANK-HEIGHT/2)
(check-expect (add-missile empty 10)
              (list (make-missile 10 (- HEIGHT TANK-HEIGHT/2))))
(check-expect (add-missile (list (make-missile 50 (- HEIGHT TANK-HEIGHT/2))
                                 (make-missile 150 (- HEIGHT TANK-HEIGHT/2))) 100)
              (list (make-missile 100 (- HEIGHT TANK-HEIGHT/2))
                    (make-missile 50 (- HEIGHT TANK-HEIGHT/2))
                    (make-missile 150 (- HEIGHT TANK-HEIGHT/2))))
; (define (add-missile lom x) lom)   ;stub
(define (add-missile lom x)
  (cons (make-missile x (- HEIGHT TANK-HEIGHT/2)) lom))

;; Game -> Boolean
;; produce true if any invader in the game reaches the bottom of screen else false
; (define (end-game? loi) false)  ;stub
(define (end-game? g)
  (if (end-game-helper? (game-invaders g))
      true
      false))


;; ListOfInvaders -> Boolean
;; produce true if any invader reaches the bottom of screen else false
(check-expect (end-game-helper? empty) false)
(check-expect (end-game-helper? (list (make-invader 50 100 5) (make-invader 200 300 7))) false)
(check-expect (end-game-helper? (list (make-invader 100 500 1) (make-invader 200 300 7))) true)
; (define (end-game? loi) false)  ;stub
(define (end-game-helper? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (end-game-helper? (rest loi)))]))

;; Game -> Image
;; produce an GAME OVER image
(check-expect (game-over G0)
              (place-image (text "GAME OVER" 36 "red")
                           (/ WIDTH 2) (/ HEIGHT 2)
                           BACKGROUND))
; (define (game-over) BACKGROUND) ;stub
(define (game-over g)
  (place-image (text "GAME OVER" 36 "red")
               (/ WIDTH 2) (/ HEIGHT 2)
               BACKGROUND))