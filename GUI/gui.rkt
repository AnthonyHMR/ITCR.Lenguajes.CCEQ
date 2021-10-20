#lang racket
(require plot racket/class racket/gui/base)
(require (lib "graphics.ss" "graphics")) ; Libreria de graficos simples
(open-graphics) ; abrir la libreria

; CREAR VENTANA DONDE VAMOS A TRABAJAR
(define ventana(open-viewport "Jueguito" 1200 600)) ; ANCHO POR ALTO
(define ventana2(open-pixmap "ventna2" 1200 600))

; DIBUJAR LA CANCHA EN LA VENTANA CREADA
; make-posn: PERMITE DAR LA POSICION DONDE QUEREMOS DIBUJARLO.
((draw-solid-rectangle ventana) (make-posn 0 0) 1200 600 "dark green")
((draw-rectangle ventana) (make-posn 0 200) 100 200 "black")
((draw-rectangle ventana) (make-posn 1100 200) 100 200 "black")
((draw-line ventana) (make-posn 600 0) (make-posn 600 600) "black")
((draw-ellipse ventana) (make-posn 450 150) 300 300 "black")

; FUNCION PARA DIBUJAR JUGADORES
(define (jugadores lst color_ numb)
  (cond ((>= (length lst) 1)
             ((draw-solid-ellipse ventana2) (make-posn (caar lst) (last(car lst))) 20 20 color_)
             (jugadores (cdr lst) color_ (+ numb 1))
             ((draw-string ventana2) (make-posn (+ (caar lst) 3) (+ (last(car lst)) 15)) (number->string numb)))))

(define (ball lst)
  ((draw-solid-ellipse ventana2) (make-posn (car lst) (last lst)) 20 20 "white"))

(define (refresh lst1 lst2 ball_ score)
  ((draw-solid-rectangle ventana2) (make-posn 0 0) 1200 600 "dark green")
  ((draw-rectangle ventana2) (make-posn 0 200) 100 200 "black")
  ((draw-rectangle ventana2) (make-posn 1100 200) 100 200 "black")
  ((draw-line ventana2) (make-posn 600 0) (make-posn 600 600) "black")
  ((draw-ellipse ventana2) (make-posn 450 150) 300 300 "black")
  ((draw-string ventana2) (make-posn 588 20) (number->string (car score)))
  ((draw-string ventana2) (make-posn 606 20) (number->string (last score)))
  (jugadores lst1 "orange" 1)
  (jugadores lst2 "red" 12)
  (ball ball_)
  
  (copy-viewport ventana2 ventana)
  ((clear-viewport ventana2)))
  
(refresh (list (list 5 290) (list 500 290)) (list (list 1175 290) (list 900 290)) (list 590 290) (list 0 0))
(sleep/yield 1)
(refresh (list (list 10 290) (list 500 290)) (list (list 1175 290) (list 900 290)) (list 590 290) (list 1 0))
(sleep/yield 1)
(refresh (list (list 15 290) (list 500 290)) (list (list 1175 290) (list 900 290)) (list 590 290) (list 1 1))
(sleep/yield 1)
(refresh (list (list 20 290) (list 505 290)) (list (list 1175 290) (list 900 290)) (list 590 290) (list 2 1))