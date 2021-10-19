#lang racket
(require "Player.rkt")

(define dna1 (dna (random 7) (random 8) 5 290 8));;instantiate dna
(define goalkeeper1(player "gk" dna1 1)) ;;instantiate goalkeeper team 1
(define team1 '(goalkeeper1)) ;;Team 1 players list

(define dna2 (dna (random 7) (random 8) 1175 290 8));;instantiate dna
(define goalkeeper2(player "gk" dna2 22)) ;;instantiate goalkeeper team 2
(define team2 '(goalkeeper2)) ;;Team 2 players list
;;generate ids list
(define (id-generator n id-list start)
  (cond ((= 0 n)
         id-list)
        (else
         (define next (add1 start))
         (id-generator (sub1 n) (append id-list (list next)) next))))
(define id-numbers (id-generator 20 '() 1)) ;;ids list for field players
id-numbers
#|------------------------------DEFENDERS TEAM 1----------------------------------------------|#
(define (init-defenders-dna numdef list-def limit)
  #| init-defenders-dna creates defender-like dna for players on team 1
     params:
           numdef: amount of defender-dna needed
           list-def: empty list that will store dna's
           limit: field limit position for a player
  |#
  (define speed (random 7))
  (define force (random 8))
  (define xpos (+ limit (random 390))) ;;x coordinate limit for defenders on team 1
  (define ypos (random 600))
  (define dna3 (dna speed force xpos ypos (random 7))) ;;dna instance
  (cond ((= 0 numdef) ;; return list-def when dna's are set
      list-def)
        (else
         (init-defenders-dna (sub1 numdef) (append list-def (list dna3)) limit)))) ;;recursive call, subtracts numdef and appends dna to list-def

(define dnas(init-defenders-dna 3 '() 10)) ;;init a list of 3 defenders dna

(dna-positionx (list-ref dnas 0));;checks x coordinate for player 1
(dna-positionx (list-ref dnas 1));;checks y coordinate for player 1
(dna-positionx (list-ref dnas 2))
(define (init-defenders-list num_dnas list-defs list-dnas role)
  #| init-defenders-list creates defender-like players on team 1
     params:
           num_dnas: amount of defender-dna to asign
           list-defs: defenders list
           list-dnas: dna list that will be asign to defenders
  |#
  (cond((= 0 num_dnas) ;;return list of defenders 
        list-defs)
       (else
        (define current_dna (list-ref list-dnas (sub1 num_dnas))) ;;gets current dna from list-dnas according to num_dnas
        (define id-number (list-ref id-numbers num_dnas))                       ;;sets id player number
        (define defender (player role current_dna (sub1 id-number))) ;;sets player info
        (init-defenders-list (sub1 num_dnas) (append list-defs (list defender)) list-dnas role)))) ;;recursive call, subtracts num_dnas, appends new defender to list-defs
        
(define defenders (init-defenders-list 3 '() dnas "def"))  
(append team1 defenders)
(player-id (list-ref defenders 0))
(player-id (list-ref defenders 1))
(player-id (list-ref defenders 2))
(player-role (list-ref defenders 0))
(player-role (list-ref defenders 1))
(player-role (list-ref defenders 2))
(get-x (list-ref defenders 0))
(get-y (list-ref defenders 0))
#|----------------------------MIDFIELDERS-------------------------------|#


