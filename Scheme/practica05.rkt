#lang r6rs
(import (rnrs)
        (schemeunit))


;; Lawrence Rider García


;;
;; Practica 5
;;


;; Ejercicio 1.a
(define (prueba x)
  (lambda (y) (+ x y)))

(define f (prueba 10))
(define g (prueba 5))

;; Demostración 1.a
(display "\n\nEjercicio 1.a\n")
(display "Al aplicar (f 3) el resultado es: ")
(display (f 3))
(display "\nAl aplicar (g 3) el resultado es: ")
(display (g 3))

;; Pruebas 1.a
(check-equal? (f 3) 13)
(check-equal? (g 3) 8)
(check-equal? (f 5) 15)
(check-equal? (g -3) 2)


;; Ejercicio 1.b.1
(define g1 (lambda (x) (+ x 8)))

;; Demostración 1.b.1
(display "\n\nEjercicio 1) b.1\n")
(display "La expresión es (g1 4) y da: ")
(display (g1 4))


;; Ejercicio 1.b.2
(define (g2 x) (lambda (y) (* x y)))

;; Demostración 1.b.2
(display "\n\nEjercicio 1) b.2\n")
(display "La expresión es ((g2 3) 4) y da: ")
(display ((g2 3) 4))


;; Ejercicio 2
(display "\n\nEjercicio 2\n")
(display "(define x 10)  ;; x Global
(define y 3)   ;; y Global
(define (prueba x)
   (let ((y (+ x 4))  ;; X Capturada de prueba (valor 2), Y valor 6
         (z (+ y 3))) ;; Y Global (valor 3), Z valor 6
      (lambda (str)   ;; Str valor \"hola\"
         (+ (string-length str) x y z)))) ;; Str local (valor \"hola\"), X capturada (valor 2),
                                          ;; Y local de let (valor 6), Z local de let (valor 6)
(define f (prueba2 2))
(f \"hola\")")


;; Ejercicio 3.a
(define (construye-conversores lista)
  (if (null? lista)
      '()
      (append (list (lambda (cantidad) (cons (* cantidad (caar lista)) (cdar lista))))
              (construye-conversores (cdr lista)))))

;; Demostración 3.a
(define lista-conversores (construye-conversores (list (cons 1.11072 'Dólar-estadounidense) (cons 0.77444 'Libra-esterlina) (cons 125.774 'Yen-japonés))))
(define precision 0.000001)
(define (iguales-reales? x y) (< (abs (- x y)) precision))
(define (iguales-monedas? x y)
  (and (iguales-reales? (car x) (car y))
       (equal? (cdr x) (cdr y))))

(display "\n\nEjercicio 3.a\n")
(display "El resultado de convertir 10€ en Libra-esterlina es: ")
(display ((cadr lista-conversores) 10))

;; Pruebas 3.a
(check-true (iguales-monedas? ((cadr lista-conversores) 10) (cons 7.7444 'Libra-esterlina)))
(check-true (iguales-monedas? ((list-ref lista-conversores 2) 10) (cons 1257.74 'Yen-japonés)))
(check-true (iguales-monedas? ((caddr lista-conversores) 4) (cons 503.096 'Yen-japonés)))
(check-true (iguales-monedas? ((car lista-conversores) 26) (cons 28.878719 'Dólar-estadounidense)))


;; Ejercicio 3.b
(define (construye-conversores-FOS lista)
  (fold-right (lambda (pareja base) 
                (append (list (lambda (cantidad) (cons (* cantidad (car pareja))
                                                       (cdr pareja))))
                        base))
              '() lista))

;; Demostración 3.b
(define lista-conversores-FOS (construye-conversores-FOS (list (cons 1.11072 'Dólar-estadounidense) (cons 0.77444 'Libra-esterlina) (cons 125.774 'Yen-japonés))))

(display "\n\nEjercicio 3.b\n")
(display "El resultado de convertir 10€ en Libra-esterlina es: ")
(display ((cadr lista-conversores-FOS) 10))

;; Pruebas 3.b
(check-true (iguales-monedas? ((cadr lista-conversores-FOS) 10) (cons 7.7444 'Libra-esterlina)))
(check-true (iguales-monedas? ((list-ref lista-conversores-FOS 2) 10) (cons 1257.74 'Yen-japonés)))
(check-true (iguales-monedas? ((caddr lista-conversores-FOS) 4) (cons 503.096 'Yen-japonés)))
(check-true (iguales-monedas? ((car lista-conversores-FOS) 26) (cons 28.878719 'Dólar-estadounidense)))


;; Ejercicio 4.a
(define (suma-iter lista)
  (suma-iter-aux 0 lista))

(define (suma-iter-aux suma lista)
  (if (null? lista)
      suma
      (suma-iter-aux (+ suma (car lista)) (cdr lista))))

;; Demostración 4.a
(display "\n\nEjercicio 4.a\n")
(display "La suma de los números de la lista '(1 2 3 4 5) es: ")
(display (suma-iter '(1 2 3 4 5)))

;; Pruebas 4.a
(check-equal? (suma-iter '(1 2 3 4 5)) 15)
(check-equal? (suma-iter '(1 -1 2 3 1)) 6)
(check-equal? (suma-iter '(0 0 0)) 0)


;; Ejercicio 4.b
(define (cuadrado-lista-iter lista)
  (cuadrado-lista-aux '() lista))

(define (cuadrado-lista-aux lista-cuadrados lista)
  (if (null? lista)
      lista-cuadrados
      (cuadrado-lista-aux (append lista-cuadrados (list (* (car lista) (car lista))))
                          (cdr lista))))

;; Demostración 4.b
(display "\n\nEjercicio 4.b\n")
(display "El cuadrado de los números de la lista '(2 3 4 5) es: ")
(display (cuadrado-lista-iter '(2 3 4 5)))

;; Pruebas 4.b
(check-equal? (cuadrado-lista-iter '(2 3 4 5)) '(4 9 16 25))
(check-equal? (cuadrado-lista-iter '(5 -2 3 4 6)) '(25 4 9 16 36))
(check-equal? (cuadrado-lista-iter '(0 0 0 0)) '(0 0 0 0))


;; Ejercicio 5
(define (asteriscos-iter n)
  (asteriscos-aux '() n))

(define (asteriscos-aux lista n)
  (if (= n 0)
      lista
      (asteriscos-aux (cons (expande-asteriscos '() n) lista) (- n 1))))

(define (expande-asteriscos lista n)
  (if (= n 0)
      lista
      (expande-asteriscos (append lista '(*)) (- n 1))))

;; Demostración 5
(display "\n\nEjercicio 5\n")
(display "La lista creada con 5 asteriscos es: ")
(display (asteriscos-iter 5))

;; Pruebas 5
(check-equal? (asteriscos-iter 5) (list (list '*) (list '* '*) (list '* '* '*) (list '* '* '* '*) (list '* '* '* '* '*)))
(check-equal? (asteriscos-iter 1) (list (list '*)))
(check-equal? (asteriscos-iter 6) (list (list '*) (list '* '*) (list '* '* '*) (list '* '* '* '*) (list '* '* '* '* '*) (list '* '* '* '* '* '*)))

