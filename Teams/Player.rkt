#lang racket

(struct dna(speed force positionx positiony shooting ball-dist)) ;;creates dna structure; desired traits 10 10 x=xball y=yball 10
(struct player(role dna id));;creates player structure

(define (get-x player)                 ;;returns x coordinate
  (dna-positionx (
               player-dna player)))
(define (get-y player)                 ;;returns y coordinate
  (dna-positiony (
               player-dna player)))
(define (get-speed player)                 
  (dna-speed (
               player-dna player)))
(define (get-force player)                
  (dna-force (
               player-dna player)))
(define (get-shooting player)                
  (dna-shooting (
               player-dna player)))
(define (get-ball-dist player)                
  (dna-ball-dist (
               player-dna player)))
#|---------------------------SET INITIAL DATA-----------------------|#
(define xtarget 590)
(define ytarget 290)

(define (pitagoras x1 y1 x2 y2)
  #|
   pitagoras: calculates the distance between two points
   params:
         x1, y1: player location
         x2, y2: ball location
|#
  (define delta_y (- y2 y1))
  (define delta_x (- x2 x1))
  (cond ((or (> 0 delta_y) (> 0 delta_x))
         (define abs_y (abs delta_y))
         (define abs_x (abs delta_x))
           (sqrt (+ (* abs_y abs_y) (* abs_x abs_x))))
        (else
         (sqrt (+ (* delta_y delta_y) (* delta_x delta_x))))))

(define (calc-ball-dist x1 y1 xtarget ytarget)
    #|
   calc-ball-dist calculates the score for a given range of distance
   params:
         x1, y1: player location
         x2, y2: ball location
|#
  (define distance (pitagoras x1 y1 xtarget ytarget))
  (cond((<= 1080 distance 1342)
        1)
       ((<= 960 distance 1080)
        2)
       ((<= 840 distance 960)
        3)
       ((<= 720 distance 840)
        4)
       ((<= 600 distance 720)
        5)
       ((<= 480 distance 600)
        6)
       ((<= 360 distance 480)
        7)
       ((<= 240 distance 360)
        8)
       ((<= 120 distance 240)
        9)
       (else
        10)))
;;(pitagoras 1200 1200 xtarget ytarget)
;(define dna1 (dna 5 5 400 200 8 (calc-ball-dist 400 200 xtarget ytarget)));;instantiate dna
;(define goalkeeper(player "gk" dna1 1 ))
;(get-ball-dist goalkeeper)
(provide (all-defined-out))
;;(get-x (car team)) ;;test