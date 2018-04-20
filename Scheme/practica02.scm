;; Lawrence Arthur Rider Garcia 

;; Practica 02

;; 
;; Ejercicio 1: forma especial y funcion
;;

;; Apartado A)

(define (new-cond expr1 then1 expr2 then2 else)  
    (cond  
        (expr1 then1)  
        (expr2 then2)  
        (else else)))

"Pruebas ejercicio 1 a)"
(cond ((= 2 1) (+ 1 1)) ((> 3 2) (+ 2 3)) (else (- 10 3))) ;; => 5 
(new-cond (= 2 1) (+ 1 1) (> 3 2) (+ 2 3) (- 10 3))        ;; => 5

;; En estas primeras pruebas, se puede observar que el resultado es el mismo, 
;; aunque en realidad no se ha evaluado de la misma forma.
;;
;; 'new-cond' es una funcion, es decir, que se evaluan antes todos los argumentos
;; y despues se invoca a la funcion.
;; En cambio, 'cond' es una forma especial y evalua antes solo las condiciones que tiene.
;;
;; Esto supone que, como vemos en las pruebas de mas abajo,'new cond' evaluaria (/ 3 0),
;; y lanza un error de division por cero, mientras que 'cond' no llega a evaluar esa
;; opcion, porque la primera es correcta y termina ahi su ejecucion.

(cond ((= 2 2) 1) ((> 3 2) 2) (else (/ 3 0)))   ;; => 1
;;(new-cond (= 2 2) 1 (> 3 2) 2 (/ 3 0))        ;; => da error division por cero


;; Apartado B)


"Pruebas ejercicio 1 b)"
(and (= 4 5) (/ 3 0))  ;; => #f como la primera condicion es falsa, no evalua la siguiente
(or (= 2 2) (/8 0))    ;; => #t como la primera es correcta, no evalua la siguiente

;; Como se puede observar, tambien son formas especiales. No tiene por que evaluar
;; todas las condiciones. En el caso de 'and', en cuanto evalua una condicion 
;; como falsa, devuelve #f y no evalua el resto. En el 'or' lo mismo, cuando evalua
;; una condicion como verdadera, no sigue evaluando el resto. 

;; 
;; Ejercicio 2: maximo lista
;;

(define (maximo lista)
  (if (null? (cdr lista))
     (car lista)
     (max (car lista) (maximo (cdr lista)))))
      
  
"Pruebas ejercicio 2"

(maximo '(16 5 3 2 1 4))  ;; => 16
(maximo '(3 8 8))         ;; => 8
(maximo '(3 11 8 24))     ;; => 24
(maximo '(1))             ;; => 1


;; 
;; Ejercicio 3: lista ordenada?
;;

;; Apartado A)

(define (ordenada? lista)
 (if (null? (cdr lista))
     #t
     (if (<= (car lista) (car(cdr lista)))
         (if (ordenada? (cdr lista))
             #t
             #f)
         #f)))
      

"Pruebas ejercicio 3 a)"
(ordenada? '(1 2 3))          ;; => #t
(ordenada? '(2 5 11 9 41))    ;; => #f
(ordenada? '(-32 5 23 59 78)) ;; => #t
(ordenada? '(8))              ;; => #t


;; Apartado B)

(define (ordenada-palabra? pal)
  (if (null? (cdr (string->list pal)))
      #t
      (if(char<=? (car (string->list pal)) (car (cdr (string->list pal))))
         (ordenada-palabra? (substring pal 1 (string-length pal)))
         #f)))
         
"Pruebas ejercicio 3 b)"

(ordenada-palabra? "fghijk")     ;; => #t
(ordenada-palabra? "holaquetal") ;; => #f


;; 
;; Ejercicio 4: intervalos
;;

;; Apartado A)


(define (engloban-intervalos? a b)
  (or (and (<= (car a) (car b)) (<= (cdr b) (cdr a))) 
      (and (<= (car b) (car a)) (<= (cdr a) (cdr b)))))

"Pruebas ejercicio 4 a)"

(engloban-intervalos? '(2 . 3) '(6 . 10))  ;; => #f
(engloban-intervalos? '(1 . 4) '(3 . 9))   ;; => #f
(engloban-intervalos? '(5 . 12) '(7 . 8))  ;; => #t
(engloban-intervalos? '(7 . 8) '(5 . 12))  ;; => #t


;; Apartado B)

(define (union-intervalos a b)
  (if (equal? a 'vacio)
      b
      (if (equal? b 'vacio)
          a
          (cons (min (car a) (car b))
                (max (cdr a) (cdr b))))))

(define (intersectan? a b) 
  (and (<= (car b) (cdr a)) (<= (car a) (cdr b))))

(define (intersectar a b)
  (cons (max (car a) (car b)) (min (cdr a) (cdr b))))

(define (interseccion-intervalos a b)
  (if (or (equal? b 'vacio) (equal? a 'vacio))
      'vacio
      (if (intersectan? a b)
          (intersectar a b)
          'vacio)))



"Pruebas ejercicio 4 b)"

(union-intervalos 'vacio 'vacio)       ;; => vacio
(union-intervalos '(2 . 3) '(6 . 10))  ;; => (2 . 10)
(union-intervalos '(6 . 12) '(4 . 5))  ;; => (4 . 12)
(union-intervalos 'vacio '(2 . 3))     ;; => (2 . 3)
(union-intervalos '(8 . 25) 'vacio)    ;; => (8 . 25)

(interseccion-intervalos '(1 . 25) 'vacio)   ;; => vacio
(interseccion-intervalos 'vacio '(3 . 9))    ;; => vacio
(interseccion-intervalos '(5 . 6) '(1 . 2))  ;; => vacio
(interseccion-intervalos '(4 . 8) '(5 . 12)) ;; => (5 . 8)
(interseccion-intervalos '(1 . 10) '(4 . 6)) ;; => (4 . 6)

;; 
;; Ejercicio 5: recursividad intervalos
;;

(define (union-lista-intervalos lista)
  (if (null? lista)
      'vacio
      (union-intervalos (car lista) (union-lista-intervalos (cdr lista)))))

(define (interseccion-lista-intervalos lista)
  (if (null? (cdr lista))   
      (car lista)
      (interseccion-intervalos (car lista) (interseccion-lista-intervalos (cdr lista)))))

"Pruebas ejercicio 5"

(union-lista-intervalos '((4 . 25) (15 . 22) (1 . 20)))    ;; => (1 . 25)
(union-lista-intervalos '((1 . 3) (-2 . 2) (1 . 1)))       ;; => (-2 . 3)
(union-lista-intervalos '((1 . 2) (2 . 6) (3 . 5)))        ;; => (1 . 6)
(interseccion-lista-intervalos '((1 . 4) (3 . 5) (8 . 9))) ;; => vacio
(interseccion-lista-intervalos '((1 . 8) (2 . 6) (4 . 5))) ;; => (4 . 5)
