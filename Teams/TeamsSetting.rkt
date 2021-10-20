#lang racket
(require "Player.rkt")

#|------------------------------ID's list generator----------------------------------------------|#
(define (id-generator n id-list start)
    #| id-generator: creates a numbers list (2...21)
     params:
           n: number of ids
           id-list: empty list
           start: init counter
     output:
           id-list: list ids
  |#
  (cond ((= 0 n)
         id-list)
        (else
         (define next (add1 start))
         (id-generator (sub1 n) (append id-list (list next)) next))))
(define players-ids (id-generator 20 '() 1)) ;;ids list for field players
#|------------------------------GOALKEEPERS----------------------------------------------|#
(define (add-gk num_gk gk_list)
  #| add-gk: creates a goalkeepers list
     params:
           num_gk: number of goalkeepers
           gk_list: empty list
     output:
           gk_list: list of goalkeepers
  |#
  (define speed (+ (random 7) 1))
  (define force (+ (random 8) 1))
  (define shooting (+ (random 7) 1))
  (cond ((= 0 num_gk)
         gk_list)
        ((= 1 num_gk)
         (define dna1 (dna speed force 1175 290 shooting (calc-ball-dist 1175 290 xtarget ytarget)))
         (define goalkeeper(player "gk" dna1 22))
         (add-gk (sub1 num_gk) (append gk_list (list goalkeeper))))
        (else
         (define dna1 (dna speed force 5 290 shooting (calc-ball-dist 5 290 xtarget ytarget)))
         (define goalkeeper(player "gk" dna1 1))
         (add-gk (sub1 num_gk) (append gk_list (list goalkeeper))))
        )
  )
(define gk_list (add-gk 2 '())) ;;create goalkeepers list

#|------------------------------DNA AND PLAYERS FUNCTIONS----------------------------------------------|#
(define (init-players-dna numdef list-def limit)
  #| init-players-dna: creates dna for players according to some parameters
     params:
           numdef: amount of player-dna needed
           list-def: empty list that will store dna's
           limit: field limit position for a player
  |#
  (define speed (+ (random 7) 1))
  (define force (+ (random 8) 1))
  (define xpos (+ limit (random 390))) ;;x coordinate limit for players
  (define ypos (+ (random 590) 10))
  (define shooting (+ (random 7) 1))
  (define dna3 (dna speed force xpos ypos shooting (calc-ball-dist xpos ypos xtarget ytarget))) ;;dna instance
  (cond ((= 0 numdef) ;; return list-def when dna's are set
      list-def)
        (else
         (init-players-dna (sub1 numdef) (append list-def (list dna3)) limit)))) ;;recursive call, subtracts numdef and appends dna to list-def


(define (init-players-list num_dnas list-defs list-dnas role id-numbers)
  #| init-players-list: creates players list
     params:
           num_dnas: amount of player-dna to asign
           list-defs: players list
           list-dnas: dna list that will be asign to defenders
           id-numbers: list of ids availables
  |#
  (cond((= 0 num_dnas) ;;return list of defenders 
        list-defs)
       (else
        (define current_dna (list-ref list-dnas (sub1 num_dnas))) ;;gets current dna from list-dnas according to num_dnas
        (define id-number (list-ref id-numbers num_dnas))                       ;;sets id player number
        (define defender (player role current_dna (sub1 id-number))) ;;sets player info
        (init-players-list (sub1 num_dnas) (append list-defs (list defender)) list-dnas role id-numbers)))) ;;recursive call, subtracts num_dnas, appends new defender to list-defs

(define (cut-list n list)
  ;;cut-list deletes the firsts n-th elements in list. It makes sure each player has diferent id
  (cond((= 0 n) list)
       (else
        (cut-list (sub1 n) (cdr list)))))
#|------------------------------HOW TO SET A TEAM: team 1 (random population)----------------------------------------------|#
(define def-dnas(init-players-dna 4 '() 10)) ;;init a list of 4 defenders dna
(define defenders (init-players-list 4 '() def-dnas "def" players-ids))

(define mid-ids (cut-list 4 players-ids))
(define mid-dnas(init-players-dna 4 '() 410)) ;;init a list of 4 midfielders dna
(define midfields (init-players-list 4 '() mid-dnas "mid" mid-ids))

(define fwd-ids (cut-list 4 mid-ids))
(define for-dnas(init-players-dna 2 '() 800)) ;;init a list of 2 forwards dna
(define forwards (init-players-list 2 '() for-dnas "fwd" fwd-ids))
(define team1 (append (list (car gk_list))defenders midfields forwards)) ;;TEAM LIST

(define team2-ids (cut-list 2 fwd-ids));;remaining ids numbers
#|------------------------------HOW TO SET A TEAM: selection----------------------------------------------|#
(define (fitness players fitlist n k)
  #|
    fitness calculates the fitness percentage value for a given list of players
    input:
          players: list of players
          fitlist: list with fitness value and players
          n: length of the list
          k: counter
    output:
          fitlist: a list of pairs (player fitness) for a given team
   |#
  (cond ((>= k n)
         fitlist)
        (else
         (define player (list-ref players k))
         (define total_points (+ (get-speed player) (get-force player) (get-shooting player) (get-ball-dist player)))
         (define fit_percentage (* 100 (/ total_points 40)))
         (fitness players (append fitlist (list (list  player fit_percentage))) n (add1 k))
        )))

;;DATA CONSULTS
(length team1)
(player-id (list-ref team1 0))
(player-id (list-ref team1 1))
(player-id (list-ref team1 2))
(player-id (list-ref team1 3))
(player-id (list-ref team1 4))
(player-id (list-ref team1 5))
(player-id (list-ref team1 6))
(player-id (list-ref team1 7))
(player-id (list-ref team1 8))
(player-id (list-ref team1 9))
(player-id (list-ref team1 10))
(player-role (list-ref team1 0))
(player-role (list-ref team1 1))
(player-role (list-ref team1 2))
(get-x (list-ref team1 0)) ;;position for goalkeeper on team1
(get-y (list-ref team1 0))
(get-ball-dist (list-ref team1 0))
(fitness team1 '() (length team1) 0);; get fitness for an individual in a team
;;(provide (all-defined-out))
#|------------------------------HOW TO SET A TEAM:team 2----------------------------------------------|#


