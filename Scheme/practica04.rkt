#lang r6rs
(import (rnrs)
        (schemeunit))


;; Lawrence Rider García


;;
;; Practica 4
;;


;; Ejercicio 1.a
(define (suma-lista-nums lista1 lista2)
  (if (null? lista1)
      '()
      (cons (+ (car lista1) (car lista2)) (suma-lista-nums (cdr lista1) (cdr lista2)))))

(define (divide-lista-nums lista1 lista2)
  (if (null? lista1)
      '()
      (cons (/ (car lista1) (car lista2)) (divide-lista-nums (cdr lista1) (cdr lista2)))))

;; Demostración 1.a
(display "\nEjercicio 1.a\n")
(display "El resultado de sumar los elementos de la lista (10 8 4 6) con (2 5 3 4) es: ")
(display (suma-lista-nums '(10 8 4 6) '(2 5 3 4)))
(display "\nEl resultado de dividir los elementos de la lista (10 8 1 8) con (2 16 3 4) es: ")
(display (divide-lista-nums '(10 8 1 8) '(2 16 3 4)))

;; Pruebas 1.a
(check-equal? (suma-lista-nums '(10 8 4 6) '(2 5 3 4)) (list 12 13 7 10))
(check-equal? (suma-lista-nums '(0 5 8) '(-2 3 1)) (list -2 8 9))
(check-equal? (suma-lista-nums '(-2 1 5 6) '(-2 -1 0 4)) (list -4 0 5 10))
(check-equal? (divide-lista-nums '(10 8 1 8) '(2 16 3 4)) (list 5 1/2 1/3 2))
(check-equal? (divide-lista-nums '(1 3 5) '(1 2 10)) (list 1 3/2 1/2))
(check-equal? (divide-lista-nums '(-5 1 0 3) '(15 16 3 -4)) (list -1/3 1/16 0 -3/4))


;; Ejercicio 1.b
(define (opera-listas funcion lista1 lista2)
  (if (null? lista1)
      '()
      (cons (funcion (car lista1) (car lista2)) (opera-listas funcion (cdr lista1) (cdr lista2)))))

;; Demostración 1.b
(display "\n\nEjercicio 1.b\n")
(display "Al operar con la funcion + las dos listas (10 8 4 6) y (2 5 3 4) da: ")
(display (opera-listas + '(10 8 4 6) '(2 5 3 4)))

;; Pruebas 1.b
(check-equal? (opera-listas + '(10 8 4 6) '(2 5 3 4)) (list 12 13 7 10))
(check-equal? (opera-listas / '(10 8 1 8) '(2 16 3 4)) (list 5 1/2 1/3 2))
(check-equal? (opera-listas - '(2 3 5 0) ' (-2 1 3 0)) (list 4 2 2 0))
(check-equal? (opera-listas * '(2 3 5 0) ' (-2 1 3 0)) (list -4 3 15 0))


;; Ejercicio 2.a
(define (aplica-funciones lista-parejas)
  (if (null? lista-parejas)
      '()
      (cons ((caar lista-parejas) (cdar lista-parejas)) (aplica-funciones (cdr lista-parejas)))))

;; Demostración 2.a
(display "\n\nEjercicio 2.a\n")
(display "Al aplicar las funciones de la lista ((even? 6) (null? '(4)) (list 8) (car '(1 2 3)) el resultado es: ")
(display (aplica-funciones (list (cons even? 6) (cons null? '(4)) (cons list 8) (cons car '(1 2 3)))))

;; Pruebas 2.a
(check-equal? (aplica-funciones (list (cons even? 6) (cons null? '(4)) (cons list 8) (cons car '(1 2 3)))) (list #t #f '(8) 1))
(check-equal? (aplica-funciones (list (cons odd? 2) (cons cdr '(#f)) (cons pair? (cons 8 9) ))) (list #f '() #t))
(check-equal? (aplica-funciones (list (cons list? 6) (cons null? '()) (cons string-length "hola"))) (list #f #t 4))


;; Ejercicio 2.b
(define (aplica-funciones-FOS lista-parejas)
  (map (lambda (pareja)
         ((car pareja) (cdr pareja))) lista-parejas))

;; Demostración 2.b
(display "\n\nEjercicio 2.b\n")
(display "Al aplicar las funciones de la lista ((even? 6) (null? '(4)) (list 8) (car '(1 2 3)) el resultado es: ")
(display (aplica-funciones-FOS (list (cons even? 6) (cons null? '(4)) (cons list 8) (cons car '(1 2 3)))))

;; Pruebas 2.b
(check-equal? (aplica-funciones-FOS (list (cons even? 6) (cons null? '(4)) (cons list 8) (cons car '(1 2 3)))) (list #t #f '(8) 1))
(check-equal? (aplica-funciones-FOS (list (cons odd? 2) (cons cdr '(#f)) (cons pair? (cons 8 9) ))) (list #f '() #t))
(check-equal? (aplica-funciones-FOS (list (cons list? 6) (cons null? '()) (cons string-length "hola"))) (list #f #t 4))


;; Ejercicio 3.a
(define (aplana-lista lista)
  (fold-right append '() lista))

;; Demostración 3.a
(display "\n\nEjercicio 3.a\n")
(display "La lista resultante de aplanar '((1 2 3) (4 5 6) (7))) es: ")
(display (aplana-lista '((1 2 3) (4 5 6) (7))))

;; Pruebas 3.a
(check-equal? (aplana-lista '((1 2 3) (4 5 6) (7))) (list 1 2 3 4 5 6 7))
(check-equal? (aplana-lista (list (list 'a #t #f) '(5) '() (list '? 'hola))) (list 'a #t #f 5 '? 'hola))
(check-equal? (aplana-lista (list '() '(8 9) () (list 'x))) (list 8 9 'x))


;; Ejercicio 3.b
(define (expande-pareja pareja)
  (if (= (cdr pareja) 0)
      '()
      (append (list (car pareja)) (expande-pareja (cons (car pareja) (- (cdr pareja) 1))))))

(define (expande lista-parejas)
  (aplana-lista (map expande-pareja lista-parejas)))

;; Demostración 3.b
(display "\n\nEjercicio 3.b\n")
(display "El resultado de expandir la lista ((#t 3) (LPP 2) ('b 4)) es: ")
(display (expande (list (cons #t 3) (cons "LPP" 2) (cons 'b 4))))

;; Pruebas 3.b
(check-equal? (expande (list (cons #t 3) (cons "LPP" 2) (cons 'b 4))) (list #t #t #t "LPP" "LPP" 'b 'b 'b 'b))
(check-equal? (expande (list (cons 0 4) (cons #f 1) (cons 'b 0))) (list 0 0 0 0 #f))
(check-equal? (expande (list (cons "holaquetal" 1) (cons 'x 2) (cons '- 3))) (list "holaquetal" 'x 'x '- '- '-))


;; Ejercicio 3.c
(define (suma-izq n pareja)
  (cons (+ n (car pareja)) (cdr pareja)))

(define (suma-der n pareja)
  (cons (car pareja) (+ n (cdr pareja))))

(define (suma-impares-pares lista-num)
  (fold-right (lambda (x pareja) (if (even? x) (suma-der x pareja) (suma-izq x pareja)))
              (cons 0 0)
              lista-num))

;; Demostración 3.c
(display "\n\nEjercicio 3.c\n")
(display "La suma de los impares y los pares de la lista (3 2 1 4 8 7 6 5) es: ")
(display (suma-impares-pares '(3 2 1 4 8 7 6 5)))

;; Pruebas 3.c
(check-equal? (suma-impares-pares '(3 2 1 4 8 7 6 5)) (cons 16 20))
(check-equal? (suma-impares-pares '(3 1 5)) (cons 9 0))
(check-equal? (suma-impares-pares '(0 0 0 0 0 0)) (cons 0 0))
(check-equal? (suma-impares-pares '(1 3 7 9 8)) (cons 20 8))


;; Ejercicio 4
(define (tamaño-string string)
  (string-length (symbol->string string)))

(define (filtra-simbolos lista-simbolos lista-num)
  (filter pair? (map (lambda (x y) (if (= (tamaño-string x) y)
                                       (cons x y)
                                       '()))
                     lista-simbolos
                     lista-num)))
;; Demostración 4
(display "\n\nEjercicio 4\n")
(display "La lista resultante de filtrar los símbolos '(este es un ejercicio de examen) con el tamaño '(2 1 2 9 1 6) es: ")
(display (filtra-simbolos '(este es un ejercicio de examen) '(2 1 2 9 1 6)))

;; Pruebas 4
(check-equal? (filtra-simbolos '(este es un ejercicio de examen) '(2 1 2 9 1 6)) (list (cons 'un 2) (cons 'ejercicio 9) (cons 'examen 6)))
(check-equal? (filtra-simbolos '(Lorem ipsum ad his scripta) '(1 5 2 6 8)) (list (cons 'ipsum 5) (cons 'ad 2)))
(check-equal? (filtra-simbolos '(Me gusta la fotografia) '(2 3 1 10)) (list (cons 'Me 2) (cons 'fotografia 10)))


;; Ejercicio 5.a
(define (generar-cartas n palo)
  (if (= n 0)
      '()
      (append (generar-cartas (- n 1) palo)  (list (cons n palo)))))

;; Demostración 5.a
(display "\n\nEjercicio 5.a\n")
(display "La lista de parejas del palo oros es: ")
(display (generar-cartas 12 'oros))

;; Pruebas 5.a
(define oros (list (cons 1 'oros) (cons 2 'oros) (cons 3 'oros) (cons 4 'oros) (cons 5 'oros) (cons 6 'oros)
                   (cons 7 'oros) (cons 8 'oros) (cons 9 'oros) (cons 10 'oros) (cons 11 'oros) (cons 12 'oros)))
(define espadas (list (cons 1 'espadas) (cons 2 'espadas) (cons 3 'espadas) (cons 4 'espadas) (cons 5 'espadas) (cons 6 'espadas)
                      (cons 7 'espadas) (cons 8 'espadas) (cons 9 'espadas) (cons 10 'espadas) (cons 11 'espadas) (cons 12 'espadas)))
(define bastos (list (cons 1 'bastos) (cons 2 'bastos) (cons 3 'bastos) (cons 4 'bastos) (cons 5 'bastos) (cons 6 'bastos)
                     (cons 7 'bastos) (cons 8 'bastos) (cons 9 'bastos) (cons 10 'bastos) (cons 11 'bastos) (cons 12 'bastos)))
(define copas (list (cons 1 'copas) (cons 2 'copas) (cons 3 'copas) (cons 4 'copas) (cons 5 'copas) (cons 6 'copas)
                    (cons 7 'copas) (cons 8 'copas) (cons 9 'copas) (cons 10 'copas) (cons 11 'copas) (cons 12 'copas)))

(check-equal? (generar-cartas 12 'oros) oros)
(check-equal? (generar-cartas 10 'bastos) (list (cons 1 'bastos) (cons 2 'bastos) (cons 3 'bastos) (cons 4 'bastos) (cons 5 'bastos) (cons 6 'bastos)
                                              (cons 7 'bastos) (cons 8 'bastos) (cons 9 'bastos) (cons 10 'bastos)))
(check-equal? (generar-cartas 12 'copas) copas)


;; Ejercicio 5.b
(define (construye-baraja n lista-palos)
  (fold-right (lambda (palo base) (append (generar-cartas n palo) base)) '() lista-palos))

;; Demostración 5.b
(display "\n\nEjercicio 5.b\n")
(display "La baraja española completa es: ")
(display (construye-baraja 12 '(oros espadas copas bastos)))

;; Pruebas 5.b
(check-equal? (construye-baraja 12 '(oros espadas copas bastos)) (append oros espadas copas bastos))
(check-equal? (construye-baraja 12 '(oros copas bastos)) (append oros copas bastos))
(check-equal? (construye-baraja 12 '(espadas bastos)) (append espadas bastos))

