#lang r6rs
(import (rnrs)
        (schemeunit)
        (graphics turtles))


;; Lawrence Rider García


;;
;; Practica 6
;;


;; Ejercicio 1.a
(turtles #t)

(define (arbol tam)
   (if (> tam 20)
       (begin
         (draw tam)
         (turn 30)
         (arbol (- tam 10))
         (turn -60)
         (arbol (- tam 10))
         (turn 30)
         (move (- 0 tam))
         )))
(turn 90)
(arbol 90)


;; Ejercicio 1.b
(define (arboles n giro tam)
  (if (> n 0)
      (begin
        (turn giro)
        (arbol tam)
        (arboles (- n 1) giro tam))))


;; Ejercicio 2.a
(define lista-a '((a b) d (c (e) (f g) h)))

(display "\nEjercicio 2.a\n")
(display "La lista se generaría así '((a b) d (c (e) (f g) h))\n")
(display "El (cadddr (caddr lista-a)) es: ")
(display (cadddr (caddr lista-a)))


;; Ejercicio 2.b
(define lista-b '(1 (6 (3) 10) (2) 8))

(display "\n\nEjercicio 2.b\n")
(display "La lista se generaría así '(1 (6 (3) 10) (2) 8)\n")
(display "El (car (cddadr lista-b)) es: ")
(display (car (cddadr lista-b)))


;; Ejercicio 3.a.1
(define (suma-lista lista)
  (if (null? lista)
      0
      (if (list? (car lista))
          (+ (suma-lista (car lista)) (suma-lista (cdr lista)))
          (+ (car lista) (suma-lista (cdr lista))))))

(display "\n\nEjercicio 3.a.1\n")
(display "La suma de la lista '(1 (2 (3 ((((4)))) 5) 6 7)) es: ")
(display (suma-lista '(1 (2 (3 ((((4)))) 5) 6 7))))

(check-equal? (suma-lista '(1 (2 (3 ((((4)))) 5) 6 7))) 28)
(check-equal? (suma-lista '(1 ((3)) ((((((((4)))))) 3) 1) (0) 0)) 12)
(check-equal? (suma-lista '((2((((((1)))3))))8 2)) 16)


;; Ejercicio 3.a.2
(define (suma-lista-FOS lista)
  (fold-right (lambda (elemento base) (if (list? elemento)
                                          (+ (suma-lista-FOS elemento) base)
                                          (+ elemento base)))
              0
              lista))

(display "\n\nEjercicio 3.a.2\n")
(display "La suma FOS de la lista '(1 (2 (3 ((((4)))) 5) 6 7)) es: ")
(display (suma-lista-FOS '(1 (2 (3 ((((4)))) 5) 6 7))))

(check-equal? (suma-lista-FOS '(1 (2 (3 ((((4)))) 5) 6 7))) 28)
(check-equal? (suma-lista-FOS '(1 ((3)) ((((((((4)))))) 3) 1) (0) 0)) 12)
(check-equal? (suma-lista-FOS '((2((((((1)))3))))8 2)) 16)


;; Ejercicio 3.b.1
(define (aplana lista)
  (if (null? lista)
      '()
      (if (list? (car lista))
          (append (aplana (car lista)) (aplana (cdr lista)))
          (append (list (car lista)) (aplana (cdr lista))))))

(display "\n\nEjercicio 3.b.1\n")
(display "La lista '(1 (2 (3) 4 (5 (6 (7) 8)))) aplanada es: ")
(display (aplana '(1 (2 (3) 4 (5 (6 (7) 8))))))

(check-equal? (aplana '(1 (2 (3) 4 (5 (6 (7) 8))))) (list 1 2 3 4 5 6 7 8))
(check-equal? (aplana '(1 a ((((((3)))))) (1) (0))) (list 1 'a 3 1 0))
(check-equal? (aplana '(t ()() (((((((56))))) 3)))) (list 't 56 3))


;; Ejercicio 3.b.2
(define (aplana-FOS lista)
  (fold-right (lambda (elemento base) (if (list? elemento)
                                          (append (aplana-FOS elemento) base)
                                          (append (list elemento) base)))
              '()
              lista))

(display "\n\nEjercicio 3.b.2\n")
(display "La lista '(1 (2 (3) 4 (5 (6 (7) 8)))) aplanada con FOS es: ")
(display (aplana-FOS '(1 (2 (3) 4 (5 (6 (7) 8))))))

(check-equal? (aplana-FOS '(1 (2 (3) 4 (5 (6 (7) 8))))) (list 1 2 3 4 5 6 7 8))
(check-equal? (aplana-FOS '(1 a ((((((3)))))) (1) (0))) (list 1 'a 3 1 0))
(check-equal? (aplana-FOS '(t ()() (((((((56))))) 3)))) (list 't 56 3))


;; Ejercicio 4.a
(define (diff-listas l1 l2)
  (if (null? l1)
      '()
      (if (list? (car l1))
          (append (diff-listas (car l1) (car l2)) (diff-listas (cdr l1) (cdr l2)))
          (if (not (equal? (car l1) (car l2)))
              (cons (cons (car l1) (car l2)) (diff-listas (cdr l1) (cdr l2)))
              (diff-listas (cdr l1) (cdr l2))))))

(display "\n\nEjercicio 4.a\n")
(display "La diferencia entre la lista '(a (b ((c)) d e) f) y '(1 (b ((2)) 3 4) f) es: ")
(display (diff-listas '(a (b ((c)) d e) f) '(1 (b ((2)) 3 4) f)))

(check-equal? (diff-listas '(a (b ((c)) d e) f) '(1 (b ((2)) 3 4) f))
              (list (cons 'a 1) (cons 'c 2) (cons 'd 3) (cons 'e 4)))
(check-equal? (diff-listas '((a b) c) '((a b) c)) '())
(check-equal? (diff-listas '(1 3 (a b) (((4)))) '(a 3 (c b) (((3)))))
              (list (cons 1 'a) (cons 'a 'c) (cons 4 3)))
(check-equal? (diff-listas '(t y () () ((((5))))) '(? u () () ((((5))))))
              (list (cons 't '?) (cons 'y 'u)))


;; Ejercicio 4.b
(define (sustituye-elem lista elem-old elem-new)
  (if (null? lista)
      '()
      (if (list? (car lista))
          (cons (sustituye-elem (car lista) elem-old elem-new) (sustituye-elem (cdr lista) elem-old elem-new))
          (if (equal? (car lista) elem-old)
              (append (list elem-new) (sustituye-elem (cdr lista) elem-old elem-new))
              (append (list (car lista)) (sustituye-elem (cdr lista) elem-old elem-new))))))

(display "\n\nEjercicio 4.b\n")
(display "La lista modificada c->h en '(a b (c d (e c)) c (f (c) g)) es: ")
(display (sustituye-elem  '(a b (c d (e c)) c (f (c) g))  'c  'h))

(check-equal? (sustituye-elem  '(a b (c d (e c)) c (f (c) g))  'c  'h)
              '(a b (h d (e h)) h (f (h) g)))
(check-equal? (sustituye-elem  '(1 4 5 (((t))) () c 4)  4  #t)
              (list 1 #t 5 (list(list(list 't))) '() 'c #t))
(check-equal? (sustituye-elem  '(1 (2) ((c g) h) 5 c (((c))))  'c  1)
              '(1 (2) ((1 g) h) 5 1 (((1)))))


;; Ejercicio 5
(define (transformar lista plantilla)
  (if (null? lista)
      '()
      (if (list? (car lista))
          (cons (transformar (car lista)  plantilla) (transformar (cdr lista) plantilla))
          (append (list (list-ref plantilla (car lista))) (transformar (cdr lista)  plantilla)))))

(display "\n\nEjercicio 5\n")
(display "La lista (hola que tal estas hoy) transformada por '((0 1) 4 (2 3)) es: ")
(display (transformar '((0 1) 4 (2 3)) '(hola que tal estas hoy)))

(check-equal? (transformar '((0 1) 4 (2 3)) '(hola que tal estas hoy))
              '((hola que) hoy (tal estas)))
(check-equal? (transformar '(1 4 3 2 6 5 (0)) '(lambda es una forma especial de Scheme))
              '(es especial forma una Scheme de (lambda)))
(check-equal? (transformar '(((1)) 4 (3 2)) '(esto es el ejercicio 5))
              '(((es)) 5 (ejercicio el)))
(check-equal? (transformar '((5) (((0))) 2 3 (4 0)) '(ya he terminado la practica 6))
              '((6) (((ya))) terminado la (practica ya)))


