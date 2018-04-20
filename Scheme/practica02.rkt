#lang r6rs
(import (rnrs base)
        (rnrs io simple)
        (schemeunit))


;; Lawrence Rider García


;;
;; Practica 1
;;


;; Ejercicio 1.a

(define (new-cond c1 x c2 y z)
  (cond
    (c1 x)
    (c2 y)
    (else z)))

;; Demostración
(display "\nEjercicio 1.a\n\n")
(display "cond y new-cond funcionan de forma distinta porque cond es una forma especial
y no evalúa todas las expresiones, por ejemplo en:
(cond ((< 2 2) 1) ((> 3 2) 2) (else (/ 3 0)))
Simplemente se sigue el orden de ejecución y al no llegar al (/ 3 0) no salta un error.
Sin embargo, en la función que hemos hecho nosotros se evalúan todas las expresiones 
antes de ser invocada y salta un error de división por 0\n\n")

;; Ejercicio 1.b

(or (= 8 8) (< 'a 3))
(and (equal? 't 'a) (- #t 3))

;; Demostración
(display "Ejercicio 1.b\n\n")
(display "And y or son formas especiales y no se evaluan todas las expresiones, 
si por ejemplo 'or' evalua la primera expresion y se cumple, no evaluará las demás
expresiones y no saltará el error:
(or (= 8 8) (< 'a 3)) La segunda parte que daría error no llega a evaluarse.
Y lo mismo pasa con 'and':
(and (equal? 't 'a) (- #t 3))
La expresión (- #t 3) no dará error porque no llega a evaluarse puesto que la primera
expresión devuelve false.\n")

;; Ejercicio 2

(define (minimo lista)
  (if (null? (cdr lista))
      (car lista)
      (if (<= (car lista) (minimo (cdr lista)))
          (car lista)
          (minimo (cdr lista)))))
            
;; Demostración
(display "\nEjercicio 2\n")
(display "El mínimo de los elementos (9 8 6 4 3) es el: ")
(display (minimo '(9 8 6 4 3)))

;; Pruebas
(check-equal? (minimo '(9 8 6 4 3)) 3)
(check-equal? (minimo '(9 8 3 6 4)) 3)
(check-equal? (minimo '(9 8 6 4 3)) 3)
(check-equal? (minimo '(9 8 3 6 4)) 3)


;; Ejercicio 3

(define (ordenada-decreciente? lista-nums)
  (if (null? (cdr lista-nums))
      #t
      (if (>= (car lista-nums) (cadr lista-nums))
        (ordenada-decreciente? (cdr lista-nums))
        #f)))

;; Demostración
(display "\n\nEjercicio 3\n")
(display "¿Está ordenada decrecientemente la lista (99 59 45 23 -1)?: ")
(display (ordenada-decreciente? '(99 59 45 23 -1)))

;; Pruebas
(check-equal? (ordenada-decreciente? '(99 59 45 23 -1)) #t)
(check-equal? (ordenada-decreciente? '(12 50 -1 293 1000)) #f)
(check-equal? (ordenada-decreciente? '(3)) #t)
(check-equal? (ordenada-decreciente? '(6 4 6)) #f)
(check-equal? (ordenada-decreciente? '(120 5 -3 -8)) #t)


;; Ejercicio 4.a

(define (engloban-intervalos? a b)
  (if (or (equal? a 'vacio) (equal? b 'vacio))
      #t
      (or (and (<= (car a) (car b)) (>= (cdr a) (cdr b)))
          (and (>= (car a) (car b)) (<= (cdr a) (cdr b))))))

;; Demostración
(display "\n\nEjercicio 4.a\n")
(display "¿Engloban los intervalos (5 9) y (4 8)?: ")
(display (engloban-intervalos? (cons 5 9) (cons 4 8)))


;; Pruebas
(define i1 (cons 4 9))
(define i2 (cons 3 10))
(define i3 (cons 12 15))
(define i4 (cons 8 19))
(check-equal? (engloban-intervalos? (cons 5 9) (cons 4 8)) #f)
(check-equal? (engloban-intervalos? i1 i2) #t)
(check-equal? (engloban-intervalos? i3 'vacio) #t)
(check-equal? (engloban-intervalos? (cons -1 8) (cons -3 8)) #t)
(check-equal? (engloban-intervalos? (cons 2 3) (cons 4 5)) #f)


;; Ejercicio 4.b

(define (union-intervalos a b)
  (if (equal? a 'vacio)
      b
      (if (equal? b 'vacio)
          a
          (cons (min (car a) (car b)) (max (cdr a) (cdr b))))))

;; Demostración
(display "\n\nEjercicio 4.b\n")
(display "La unión entre los intervalos (4 10) y (3 8) es: ")
(display (union-intervalos (cons 4 10) (cons 3 8)))


;; Pruebas
(check-equal? (union-intervalos (cons 4 10) (cons 3 8)) (cons 3 10))
(check-equal? (union-intervalos i2 i3) (cons 3 15))
(check-equal? (union-intervalos 'vacio i4) (cons 8 19))
(check-equal? (union-intervalos 'vacio 'vacio) 'vacio)
(check-equal? (union-intervalos (cons -2 5) (cons 1 8)) (cons -2 8))


;; Ejercicio 4.c

(define (intersectan? a b)
  (if (or (equal? a 'vacio) (equal? b 'vacio))
      #f
      (and (<= (car b) (cdr a)) (<= (car a) (cdr b)))))

(define (intersectar a b)
  (cons (max (car a) (car b)) (min (cdr a) (cdr b))))

(define (interseccion-intervalos a b)
  (if (intersectan? a b)
      (intersectar a b)
      'vacio))

;; Demostración
(display "\n\nEjercicio 4.c\n")
(display "¿Cuál es la intersección entre los intervalos (4 10) y (8 15)?: ")
(display (interseccion-intervalos (cons 4 10) (cons 8 15)))

;; Pruebas
(check-equal? (interseccion-intervalos (cons 4 10) (cons 8 15)) (cons 8 10))
(check-equal? (interseccion-intervalos i1 i3) 'vacio)
(check-equal? (interseccion-intervalos 'vacio i4) 'vacio)
(check-equal? (interseccion-intervalos (cons 1 5) (cons 6 8)) 'vacio)
(check-equal? (interseccion-intervalos (cons -5 6) (cons -8 5)) (cons -5 5))


;; Ejercicio 5

(define (union-lista-intervalos lista-intervalos)
  (if (null? (cdr lista-intervalos))
      (car lista-intervalos)
      (union-intervalos (car lista-intervalos)
                        (union-lista-intervalos (cdr lista-intervalos)))))

(define (interseccion-lista-intervalos lista-intervalos)
  (if (null? (cdr lista-intervalos))
      (car lista-intervalos)
      (interseccion-intervalos (car lista-intervalos)
                               (interseccion-lista-intervalos (cdr lista-intervalos)))))

;; Demostración
(display "\n\nEjercicio 5\n")
(display "¿Cuál es la unión entre la lista de intervalos {(2 12),(-1 10),(8 20)}?: ")
(display (union-lista-intervalos (list (cons 2 12) (cons -1 10) (cons 8 20))))
(display "\n\n¿Cuál es la intersección entre la lista de intervalos {(12 30),(-8 2),(13 35)}?: ")
(display (interseccion-lista-intervalos (list (cons 12 30) (cons -8 20) (cons 13 35))))

;;Pruebas
(check-equal? (union-lista-intervalos (list (cons 2 12) (cons -1 10) (cons 8 20))) (cons -1 20))
(check-equal? (union-lista-intervalos (list 'vacio (cons 1 5) (cons -3 6) (cons 1 7))) (cons -3 7))
(check-equal? (union-lista-intervalos (list 'vacio (cons -1 9) 'vacio)) (cons -1 9))

(check-equal? (interseccion-lista-intervalos (list (cons 12 30) (cons -8 20) (cons 13 35))) (cons 13 20))
(check-equal? (interseccion-lista-intervalos (list (cons 25 30) (cons -8 20) (cons 13 35))) 'vacio)
(check-equal? (interseccion-lista-intervalos (list (cons 1 8) (cons -3 5) (cons -1 2))) (cons 1 2))
(check-equal? (interseccion-lista-intervalos (list (cons 3 5) (cons 6 20) (cons -1 0))) 'vacio)