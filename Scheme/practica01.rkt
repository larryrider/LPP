#lang r6rs
(import (rnrs base)
        (rnrs io simple)
        (schemeunit))


;; Lawrence Rider García


;;
;; Practica 1
;;


;; Ejercicio 1

(define (mayor-de-tres n1 n2 n3)
  (if (>= n1 n2)
      (if (> n1 n3)
          n1
          n3)
      (if (>= n2 n3)
          n2
          n3)))

;; Demostracion
(display "\nEjercicio 1\n")
(display "El mayor entre los 3 numeros (2 8 1) es: ")
(display (mayor-de-tres 2 8 1))

;; Pruebas
(check-equal? (mayor-de-tres 2 8 1) 8)
(check-equal? (mayor-de-tres 3 0 3) 3)
(check-equal? (mayor-de-tres 5 1 2) 5)
(check-equal? (mayor-de-tres 1 4 9) 9)


;; Ejercicio 2

(define precision 0.000001)

(define (iguales-reales? x y)
  (< (abs (- x y)) precision))

(define (cuadrado n)
  (* n n))

(define (distancia-euclidea p1 p2)
  (sqrt (+ (cuadrado (- (car p2) (car p1))) (cuadrado (- (cdr p2) (cdr p1))))))

;; Demostracion
(display "\n\nEjercicio 2\n")
(display "La distancia euclidea entre (0 4) y (0 10) es: ")
(display (distancia-euclidea (cons 0 4) (cons 0 10)))


;; Pruebas
(check-true (iguales-reales? (distancia-euclidea (cons 0 4) (cons 0 10)) 6.0))
(check-true (iguales-reales? (distancia-euclidea (cons -2 5) (cons 9 7)) 11.180339))
(check-true (iguales-reales? (distancia-euclidea (cons 5 8) (cons 3 3)) 5.385164))
(check-true (iguales-reales? (distancia-euclidea (cons 2 -3) (cons -2 -5)) 4.472135))


;; Ejercicio 3

(define (engloba? a1 a2 b1 b2)
  (or (and (<= a1 b1) (>= a2 b2)) (and (>= a1 b1) (<= a2 b2))))

;; Demostracion
(display "\n\nEjercicio 3\n")
(display "¿Engloba el intervalo (4 10) al (5 9): ")
(display (engloba? 4 10 5 9))


;; Pruebas
(check-equal? (engloba? 4 10 5 9) #t) ;; ⇒ #t
(check-equal? (engloba? 4 9 4 15) #t) ;; ⇒ #t
(check-equal? (engloba? 2 6 4 8) #f) ;; ⇒ #f
(check-equal? (engloba? 1 5 2 3) #t)
(check-equal? (engloba? 0 3 1 5) #f)


;; Ejercicio 4a

(define (intersectan? a1 a2 b1 b2) 
  (and (<= b1 a2) (<= a1 b2)))
  
;; Demostracion
(display "\n\nEjercicio 4.a\n")
(display "¿Intersecta el intervalo (4 7) al (5 12): ")
(display (intersectan? 4 7 5 12))

;; Pruebas
(check-equal? (intersectan? 4 7 5 12) #t)
(check-equal? (intersectan? 4 9 12 15) #f)
(check-equal? (intersectan? 2 5 5 8) #t)
(check-equal? (intersectan? 3 7 1 2) #f)
(check-equal? (intersectan? 2 5 3 22) #t)

;; Ejercicio 4b

(define (intersectar a1 a2 b1 b2)
  (cons (max a1 b1) (min a2 b2)))
          
(define (interseccion a1 a2 b1 b2)
  (if (intersectan? a1 a2 b1 b2)
     (intersectar a1 a2 b1 b2)
     '()))
  
;; Demostracion
(display "\n\nEjercicio 4.b\n")
(display "La interseccion entre el intervalo (4 7) y el (5 12): ")
(display (interseccion 4 7 5 12))

;; Pruebas
(check-equal? (interseccion 4 7 5 12) '(5 . 7))
(check-equal? (interseccion 4 9 12 15) '())
(check-equal? (interseccion 2 5 5 8) '(5 . 5))
(check-equal? (interseccion 5 10 -2 2) '())
(check-equal? (interseccion 0 5 3 8) '(3 . 5))


;; Ejercicio 5

(define (white red green blue)
  (max (/ red 255) (/ green 255) (/ blue 255)))

(define (cmy red green blue color)
  (/ (- (white red green blue) (/ color 255)) (white red green blue)))


(define (rgb->cmyk red green blue)
  (if (and (= red 0) (= green 0) (= blue 0))
      '(0 0 0 1)
      (list (cmy red green blue red)
            (cmy red green blue green)
            (cmy red green blue blue)
            (- 1 (white red green blue)))))

;; Demostracion
(display "\n\nEjercicio 5\n")
(display "La conversion de rgb->cmyk de (75 0 130) es : ")
(display (rgb->cmyk 75 0 130))

;; Pruebas
(check-equal? (rgb->cmyk 75 0 130) '(11/26 1 0 25/51))
(check-equal? (rgb->cmyk 150 10 255) '(7/17 49/51 0 0))
(check-equal? (rgb->cmyk 255 255 255) '(0 0 0 0))
(check-equal? (rgb->cmyk 0 0 0) '(0 0 0 1))

