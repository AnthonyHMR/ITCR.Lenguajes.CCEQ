#lang racket

(struct dna(speed force positionx positiony shooting)) ;;creates dna structure
(struct player(role dna id));;creates player structure
#|
(define dna1 (dna 5 5 '(10 20) 8));;instantiate dna
(define dna2 (dna 5 5 '(20 20) 5))
(define dna3 (dna 5 5 '(30 20) 9))
(define goalkeeper(player "gk" dna1 1 )) ;;instantiate player
(define defender(player "def" dna2 5 ))
(define midfielder(player "md" dna3 10 ))
(define team (list goalkeeper defender midfielder)) ;;creates team list
(player-role (car team)) ;;returns 1st player role
(player-role (cadr team)) ;;returns 2nd player role
(player-role (caddr team)) ;;returns 3rd player role
(dna-position (
               player-dna (car team))) ;;returns a list (x y) which is the player's position
|#
(define (get-x player)                 ;;returns x coordinate
  (dna-positionx (
               player-dna player)))

(provide (all-defined-out))
;;(get-x (car team)) ;;test