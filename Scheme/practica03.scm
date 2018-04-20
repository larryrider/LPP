;; Lawrence Arthur Rider Garcia 

;; Practica 03

;; 
;; Ejercicio 1: incrementar parejas
;;

;; Apartado A)


(define (inc-izquierda pareja)
  (cons (+ (car pareja) 1) (cdr pareja)))

(define (inc-derecha pareja)
  (cons (car pareja) (+ (cdr pareja) 1)))
    
    
    
"Pruebas ejercicio 1 a)"
(inc-izquierda '(2 . 4))   ;; => (3 . 4)
(inc-derecha '(2 . 4))     ;; => (2 . 5)


;; Apartado B)

(define (cuenta-pares-impares lista)
  (if (null? lista)
    (cons 0 0)
    (if(even? (car lista) ) 
       (inc-izquierda (cuenta-pares-impares (cdr lista)))
       (inc-derecha (cuenta-pares-impares (cdr lista))))))

"Pruebas ejercicio 1 b)"
(cuenta-pares-impares '(2 4 1 3 3 5 9))   ;; => (2 . 5)
(cuenta-pares-impares '(2 4 2 0 2 2 24))  ;; => (7 . 0)

;; Apartado C)


(define (sumar-izquierda pareja numero)
  (cons (+ (car pareja) numero) (cdr pareja)))

(define (sumar-derecha pareja numero)
  (cons (car pareja) (+ (cdr pareja) numero)))


(define (suma-pares-impares lista)
  (if (null? lista)
    (cons 0 0)
    (if(even? (car lista) ) 
       (sumar-izquierda (suma-pares-impares (cdr lista)) (car lista))
       (sumar-derecha (suma-pares-impares (cdr lista)) (car lista)))))
  
  
"Pruebas ejercicio 1 c)"
(suma-pares-impares '(2 4 1 3))  ;; => (6 . 4)
(suma-pares-impares '(5 7 2 0))  ;; => (2 . 12)
  
  
  
;; 
;; Ejercicio 2: box & pointer
;;

(define p1 (cons 'b
                 (cons 'c '())))
(define p2 (cons 'd
                 (cons 'e 'f)))
(define p3 (cons 'a
                 (cons p1
                       (list p2 (cons 'g '())))))
  

;; 
;; Ejercicio 3: mayor que
;;

(define (mayor-que n lista)
  (if (null? lista)
      '()
      (cons (> (car lista) n) (mayor-que n (cdr lista)))))







  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  