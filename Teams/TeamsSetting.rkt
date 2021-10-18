#lang racket
(require "Player.rkt")
(define id-team1 12)
(define id-team2 23)
(define dna1 (dna 5 5 5 290 8));;instantiate dna
(define goalkeeper1(player "gk" dna1 1)) ;;instantiate player
(define team1 '(goalkeeper1))

(define dna2 (dna (random 7) (random 8) (random 401) (random 600) 8));;instantiate dna
(define goalkeeper2(player "gk" dna2 99)) ;;instantiate player

(define (init-defenders-dna numdef list-def)
  (define speed (random 7))
  (define force (random 8))
  (define xpos (random 400))
  (define ypos (random 600))
  (define dna3 (dna speed force xpos ypos (random 7))) 
  (cond ((= 0 numdef)
      list-def)
        (else
         (init-defenders-dna (sub1 numdef) (append list-def (list dna3))))))
(define dnas(init-defenders-dna 3 '())) ;;init a list of defenders dna

(dna-positionx (list-ref dnas 0));;checks x coordinate
(dna-positiony (list-ref dnas 0));;checks y coordinate
(define (init-defenders-list num_dnas list-defs list-dnas)
  
  (cond((= 0 num_dnas)
        list-defs)
       (else
        (define current_dna (list-ref list-dnas (sub1 num_dnas)))
        (define id-number (+ 3 (random 5)))
        (define defender (player "def" current_dna (sub1 id-number)))
        (init-defenders-list (sub1 num_dnas) (append list-defs (list defender)) list-dnas))))
        
(define defenders (init-defenders-list 3 '() dnas))  

(player-id (list-ref defenders 0))
(player-id (list-ref defenders 1))
(player-id (list-ref defenders 2))