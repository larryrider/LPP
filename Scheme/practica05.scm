;; Lawrence Arthur Rider Garcia 

;; Practica 05

;; 
;; Ejercicio 1.
;;

;; Apartado a)

;; quick-sort(lista) = quick-sort(lista menores) + primer elemento + quick-sort(lista mayores)
;; quick-sort(lista vacia) = lista vacia


;;Apartado b)

(define (menores x lista)
  (if (null? lista)
      '()
      (if (<= (car lista) x)
          (cons (car lista) (menores x (cdr lista)))
          (menores x (cdr lista)))))

(define (mayores x lista)
  (if (null? lista)
      '()
      (if (> (car lista) x)
          (cons (car lista) (mayores x (cdr lista)))
          (mayores x (cdr lista)))))

(define (quick-sort lista)
  (if (null? lista)
      '()
      (append (quick-sort (menores (car lista) (cdr lista))) 
              (list (car lista)) 
              (quick-sort (mayores (car lista) (cdr lista))))))


"Pruebas Ejercicio 1"

(quick-sort '(5 8 9 2 2 3))    ;; => (2 2 3 5 8 9)
(quick-sort '())               ;; => '()


;;
;; Ejercicio 2
;;

(require graphics/turtles)
(turtles #t)

(define (arbol long)
    (if (> long 0)
        (begin 
          (draw long)
          (turn 30)
          (arbol (- long 10))
          (turn -60)
          (arbol (- long 10)) 
          (turn 30)
          (move (* -1 long)))))


;; Para mostrar este arbol solo habria que poner (arbol 80) por ejemplo

;; 
;; Ejercicio 3
;;

;;Apartado a)

(define (trazo lado angulo)
  (draw lado)
  (turn angulo))

(define (espiral lado inc angulo lado-max)
  (if (< lado lado-max)
       (cons (trazo lado angulo) (espiral (+ lado inc) inc angulo lado-max))))


;; Para mostrar la espiral bastaria con poner (espiral 3 2 60 200)

;;Apartado b)

(define (cuadrado n)
  (draw n)
  (turn 90)
  (draw n)
  (turn 90)
  (draw n)
  (turn 90)
  (draw n)
  (turn 90))

(define (cuadrado-girado lado giro)
    (turn giro)
    (cuadrado lado))

(define (girador lado inc angulo angulo-max)
  (if (< angulo angulo-max)
      (cons (cuadrado-girado lado angulo) (girador (+ lado inc) inc angulo (- angulo-max angulo)))
      (cuadrado-girado lado angulo-max)))



;; Para mostrar esta especie de espiral basta con poner (girador 2 3 7 360)


;;
;; Ejercicio 4
;;


(define (suma-iter lista)
  (suma-lista lista '() 0))


(define (suma-lista lista lista-res x)
  (if (null? lista)
      lista-res
      (if (null? (cdr lista))
         (+ x (car lista))
         (+ x (suma-lista (cdr lista) lista-res (car lista))))))


"Pruebas ejercicio 4"

(suma-iter '(1 1 1 1))             ;; => 4
(suma-iter '(1 2 3 4))             ;; => 10


;;
;; Ejercicio 5
;;

(define (cumple-predicados lista n)
  (cumple-pred lista n '()))

(define (cumple-pred lista n lista-res)
  (if (null? lista) 
       lista-res
      (cumple-pred (cdr lista) n (append lista-res (list ((car lista) n))))))


"Pruebas ejercicio 5"
(cumple-predicados (list even? null? char? number?) 6)       ;; => (#t #f #f #t)
(cumple-predicados (list list? symbol? number?) 'a)          ;; => (#f #t #f)





;;
;; Ejercicio 6
;;

;;Forma recursiva

(define (list-reverse lista)
    (if (null? lista)
        '()
        (append (list-reverse (cdr lista))
                (list (car lista)))))

"Pruebas ejercicio 6 a)"

(list-reverse '(8 9 10 15))     ;; => (15 10 9 8)
(list-reverse '(-2 5 8 20))     ;; => (20 8 5 -2)


;;Forma iterativa

(define (list-reverse-iter lista result)
  (if (null? lista)
      result
      (list-reverse-iter (cdr lista) (cons (car lista) result ))))

(define (lista-reverse lista)
  (list-reverse-iter lista '()))

"Pruebas ejercicio 6 b)"

(lista-reverse '(8 9 10 15))     ;; => (15 10 9 8)
(lista-reverse '(-2 5 8 20))     ;; => (20 8 5 -2)

