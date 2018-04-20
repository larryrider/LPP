#lang r6rs
(import (rnrs base)
        (rnrs io simple)
        (schemeunit))


;; Lawrence Rider García


;;
;; Practica 3
;;


;; Ejercicio 1.a

(define p1
  (cons (cons 1 2)
        (cons 3 '())))

(define p2
  (cons (cons 'a
              (cons 'b '()))
        '()))

(define p3 
  (cons 4
        (cons p1 
              (cons p2 
                    5))))

;; Demostración 1.a
(display "\nEjercicio 1.a\n")
(display "P1: ")
(display p1)
(display "\nP2: ")
(display p2)
(display "\nP3: ")
(display p3)

;; Ejercicio 1.b
(display "\n\nEjercicio 1.b\n")
(display "P1 sí que es una lista porque el último elemento es una lista vacía, tiene dos elementos (una pareja y un número)\n")
(display "P2 también es una lista porque su último elemento es una lista vacía, tiene un elemento (una lista con dos elementos)\n")
(display "P3 no es una lista puesto que su último elemento es un número (en realidad es una pareja)")

;; Ejercicio 1.c
(display "\n\nEjercicio 1.c\n")
(display "La primera expresión (cadadr p3): ")
(display (cadadr p3))
(display "\nLa segunda expresión (cdddr p3): ")
(display (cdddr p3))


;; Ejercicio 2.a

(define (intercambia-elem pareja)
  (cons (cdr pareja) (car pareja)))

;; Demostración 2.a
(display "\n\nEjercicio 2.a\n")
(display "El intercambio entre la pareja (10 5) es: ")
(display (intercambia-elem (cons 10 5)))

;; Pruebas 2.a
(check-equal? (intercambia-elem (cons 10 5)) (cons 5 10))
(check-equal? (intercambia-elem (cons 'a -8)) (cons -8 'a))
(check-equal? (intercambia-elem (cons #t '())) (cons '() #t))


;; Ejercicio 2.b

(define (suma-izq n pareja)
  (cons (+ n (car pareja)) (cdr pareja)))

(define (suma-der n pareja)
  (cons (car pareja) (+ n (cdr pareja))))

;;Demostración 2.b
(display "\n\nEjercicio 2.b\n")
(display "Al sumar 5 a la izquierda de la pareja (10 20) el resultado es: ")
(display (suma-izq 5 (cons 10 20)))
(display "\nAl sumar 6 a la derecha de la pareja (10 20) el resultado es: ")
(display (suma-der 6 (cons 10 20)))

;;Pruebas 2.b
(check-equal? (suma-izq 5 (cons 10 20)) (cons 15 20))
(check-equal? (suma-izq 10 (cons 1 8)) (cons 11 8))
(check-equal? (suma-izq -8 (cons 8 3)) (cons 0 3))
(check-equal? (suma-der 6 (cons 10 20)) (cons 10 26))
(check-equal? (suma-der 0 (cons 1 2)) (cons 1 2))
(check-equal? (suma-der -3 (cons 5 7)) (cons 5 4))


;; Ejercicio 2.c

(define (suma-impares-pares lista-num)
  (if (null? lista-num)
      (cons 0 0)
      (if (even? (car lista-num))
          (suma-der (car lista-num) (suma-impares-pares (cdr lista-num)))
          (suma-izq (car lista-num) (suma-impares-pares (cdr lista-num))))))

;; Demostración 2.c
(display "\n\nEjercicio 2c\n")
(display "La suma de los impares y los pares de la lista (3 2 1 4 8 7 6 5) es: ")
(display (suma-impares-pares '(3 2 1 4 8 7 6 5)))

;; Pruebas 2.c
(check-equal? (suma-impares-pares '(3 2 1 4 8 7 6 5)) (cons 16 20))
(check-equal? (suma-impares-pares '(3 1 5)) (cons 9 0))
(check-equal? (suma-impares-pares '(0 0 0 0 0 0)) (cons 0 0))
(check-equal? (suma-impares-pares '(1 3 7 9 8)) (cons 20 8))


;; Ejercicio 3

(define (multiplo-de n lista-nums)
  (if (null? (cdr lista-nums))
      (list (= (mod (car lista-nums) n) 0))
      (append (list (= (mod (car lista-nums) n) 0)) (multiplo-de n (cdr lista-nums)))))

;; Demostración 3
(display "\n\nEjercicio 3\n")
(display "¿Es 10 múltiplo de los números de la lista (100 23 10 300 48 7) ?: ")
(display (multiplo-de 10 '(100 23 10 300 48 7)))

;; Pruebas 3
(check-equal? (multiplo-de 10 '(100 23 10 300 48 7)) '(#t #f #t #t #f #f))
(check-equal? (multiplo-de 2 '(3 6 8 1 5 9)) '(#f #t #t #f #f #f))
(check-equal? (multiplo-de 3 '(3 6 8 1 5 9)) '(#t #t #f #f #f #t))


;; Ejercicio 4.a

(define (tamaño-string string)
  (string-length (symbol->string string)))


(define (filtra-simbolos lista-simbolos lista-num)
  (if (null? lista-num)
      '()
      (if (= (tamaño-string (car lista-simbolos)) (car lista-num))
          (append (list (cons (car lista-simbolos) (car lista-num))) 
                  (filtra-simbolos (cdr lista-simbolos) (cdr lista-num)))
          (filtra-simbolos (cdr lista-simbolos) (cdr lista-num)))))
          
;; Demostración 4.a
(display "\n\nEjercicio 4.a\n")
(display "La lista resultante de filtrar los símbolos '(este es un ejercicio de examen) con el tamaño '(2 1 2 9 1 6) es: ")
(display (filtra-simbolos '(este es un ejercicio de examen) '(2 1 2 9 1 6)))

;; Pruebas
(check-equal? (filtra-simbolos '(este es un ejercicio de examen) '(2 1 2 9 1 6)) (list (cons 'un 2) (cons 'ejercicio 9) (cons 'examen 6)))
(check-equal? (filtra-simbolos '(Lorem ipsum ad his scripta) '(1 5 2 6 8)) (list (cons 'ipsum 5) (cons 'ad 2)))
(check-equal? (filtra-simbolos '(Me gusta la fotografia) '(2 3 1 10)) (list (cons 'Me 2) (cons 'fotografia 10)))

;; Ejercicio 4.b

(define (expande-pareja pareja)
  (if (= (cdr pareja) 0)
      '()
      (append (list (car pareja)) (expande-pareja (cons (car pareja) (- (cdr pareja) 1))))))

(define (expande lista-parejas)
  (if (null? lista-parejas)
      '()
      (append (expande-pareja (car lista-parejas)) (expande (cdr lista-parejas)))))

;; Demostración 4.b
(display "\n\nEjercicio 4.b\n")
(display "La lista expandida de ((#t 3) (LPP 2) (b 4)) es: ")
(display (expande (list (cons #t 3) (cons "LPP" 2) (cons 'b 4))))

;; Pruebas 4.b
(check-equal? (expande (list (cons #t 3) (cons "LPP" 2) (cons 'b 4))) '(#t #t #t "LPP" "LPP" b b b b))
(check-equal? (expande (list (cons -1 4) (cons '() 3) (cons #f 2))) '(-1 -1 -1 -1 () () () #f #f))
(check-equal? (expande (list (cons '* 1) (cons "hola" 3) (cons 0 1))) '(* "hola" "hola" "hola" 0))

;; Ejercicio 5

;; (define (suma-izq-lista lista-parejas)
;;   (if (null? lista-parejas)
;;       (cons 0 0)
;;       (if (odd? (caar lista-parejas))
;;           (suma-izq (caar lista-parejas) (suma-izq-lista (cdr lista-parejas)))
;;           (suma-izq-lista (cdr lista-parejas)))))
  
;; (define (suma-der-lista lista-parejas)
;;   (if (null? lista-parejas)
;;       (cons 0 0)
;;       (if (even? (cdar lista-parejas))
;;           (suma-der (cdar lista-parejas) (suma-der-lista (cdr lista-parejas)))
;;           (suma-der-lista (cdr lista-parejas)))))

;; (define (suma-parejas-impar-par lista-parejas)
;;       (cons (car (suma-izq-lista lista-parejas)) (cdr (suma-der-lista lista-parejas))))


;;;;;;;;; PREGUNTAR si lo de arriba estaría bien hecho o no, porque aunque funciona bien
;;;;;;;;; en la funcion principal no se hace la recursividad

(define (suma-izq-der pareja1 pareja2)
  (cons (+ (car pareja1) (car pareja2)) (+ (cdr pareja1) (cdr pareja2))))


(define (suma-parejas-impar-par lista-parejas)
  (if (null? lista-parejas)
      (cons 0 0)
      (cond
        ((and (odd? (caar lista-parejas)) (even? (cdar lista-parejas))) (suma-izq-der (car lista-parejas) (suma-parejas-impar-par (cdr lista-parejas))))
        ((odd? (caar lista-parejas)) (suma-izq (caar lista-parejas) (suma-parejas-impar-par (cdr lista-parejas))))
        ((even? (cdar lista-parejas)) (suma-der (cdar lista-parejas) (suma-parejas-impar-par (cdr lista-parejas))))
        (else (suma-parejas-impar-par (cdr lista-parejas))))))

;; Demostración 5
(display "\n\nEjercicio 5\n")
(display "La suma de los pares e impares de las parejas ((3 2) (6 5) (7 4)) es: ")
(display (suma-parejas-impar-par (list (cons 3 2) (cons 6 5) (cons 7 4))))

;; Pruebas 5
(check-equal? (suma-parejas-impar-par (list (cons 3 2) (cons 6 5) (cons 7 4))) (cons 10 6))
(check-equal? (suma-parejas-impar-par (list (cons 1 5) (cons 4 9) (cons 8 3))) (cons 1 0))
(check-equal? (suma-parejas-impar-par (list (cons 0 3) (cons 12 11) (cons 6 1))) (cons 0 0))
(check-equal? (suma-parejas-impar-par (list (cons 3 1) (cons 4 6) (cons 1 1))) (cons 4 6))
