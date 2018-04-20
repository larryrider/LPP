;; Lawrence Arthur Rider Garcia 

;;
;; Practica 06
;;

;; Funciones sobre listas

(define (hoja? dato)
   (not (list? dato)))

;; Funciones sobre árboles

(define (make-tree dato lista-hijos)
  (cons dato lista-hijos))
(define (make-hoja-tree dato) 
  (make-tree dato '()))
(define (dato-tree tree)
  (car tree))
(define (hijos-tree tree)
  (cdr tree))
(define (hoja-tree? tree)
  (null? (hijos-tree tree)))

;;
;; Ejercicio 1.
;;

;; Apartado a)

(define lista '((a b) d (c (e f) (g) h)))

"Pruebas Ejercicio 1"
"Apartado a)" 

lista

;; Apartado b)

(define arbol '(40 (5 (2) (3)) (10) (20 (12) (15 (13) (14)) (17) (19 (18)))))

"Apartado b)"

arbol

;;
;; Ejercicio 2.
;;

;; Apartado a) (suma-lista) y (suma-tree)

(define lista '(1 (2 (3 ((((4)))) 5) 6)))
(define tree '(3 (5 (6) (7)) (4)))


(define (suma-lista lista)
  (cond
    ((null? lista) 0)
    ((hoja? lista) lista)
    (else (+ (suma-lista (car lista)) (suma-lista (cdr lista))))))

(define (suma-bosque bosque)
  (if (null? bosque)
      0
      (+ (suma-tree (car bosque)) (suma-bosque (cdr bosque)))))

(define (suma-tree tree)
  (+ (dato-tree tree) (suma-bosque (hijos-tree tree))))

"Pruebas ejercicio 2"

"Apartado a) (suma-lista)"

(suma-lista lista)                      ;; -> 21
(suma-lista tree)                       ;; -> 25
(suma-lista '(1 2 ((3 (4)) 5) (((1))))) ;; -> 16
(suma-lista '(4 (3 (5) (6 (1)))))       ;; -> 19, es un arbol

"Apartado a) (suma-tree)"

(suma-tree tree)                 ;; -> 25
(suma-tree '(4 (3 (5) (6 (1))))) ;; -> 19, es un arbol
;;(suma-tree lista)              ;; -> error


;; Apartado b) (aplana lista)

(define (aplana lista)
  (cond 
    ((null? lista) '())
    ((hoja? lista) (list lista))
    (else (append (aplana (car lista)) 
                (aplana (cdr lista))))))

"Apartado b) (aplana lista)"

(aplana lista)            ;; -> (1 2 3 4 5 6 7)
(aplana tree)             ;; -> (3 5 6 7 4)
(aplana '((((((x)))))))   ;; -> (x)
(aplana '())              ;; -> ()
(aplana '((((5) 4) p *))) ;; -> (5 4 p *)

;; Apartado c) (diff-listas l1 l2)

(define (diff-listas l1 l2)
  (if (and (null? l1) (null? l2))
    '()
    (append
     (if (not (equal? (car (aplana l1)) (car (aplana l2))))
        (list (cons (car (aplana l1)) 
                      (car (aplana l2))))
        '())
     (diff-listas (cdr (aplana l1)) (cdr (aplana l2))))))
  
"Apartado c) (diff-listas l1 l2)"

(diff-listas '(a (b ((c)) d e) f) '(1 (b ((2)) 3 4) f)) ;; -> ((a . 1) (c . 2) (d . 3) (e . 4))
(diff-listas '(1) '(1))             ;; -> ()
(diff-listas '((a b) f) '((a b) f)) ;; -> ()
(diff-listas '((a d) f) '((* b) 5)) ;; -> ((a . *) (d . b) (f . 5))

;;
;; Ejercicio 3 
;;

;; Apartado a) (ordenado-tree? tree)

(define (ordenado-tree? tree)
  (if (hoja-tree? tree)
      #t
      (if (and (ordenado?(hijos-dir tree))(hijos-menores (dato-tree tree) (hijos-dir tree)))
         (aplica-f ordenado-tree? (hijos-tree tree))
         #f)))

(define (ordenado? lista)
  (if (null? (cdr lista))
     #t 
     (if (<= (car lista) (car(cdr lista)))
         (ordenado? (cdr lista))
         #f)))

(define (hijos-dir lista)     
  (if (null? (cdr lista))
      '()   
      (append (list (caadr lista))(hijos-dir (cdr lista)))))

(define (hijos-menores num lista)
  (if (null? (cdr lista))
     (if (> num (car lista))
        #t
        #f)
     (if (> num (car lista))
        (and (hijos-menores num (cdr lista)) #t)
        #f)))

(define (aplica-f f lista)
  (if (null? (cdr lista))
     (f (car lista))
     (and (f (car lista)) (aplica-f f (cdr lista)))))

"Pruebas ejercicio 3"
"Apartado a) (oredenado-tree?)"

(ordenado-tree? '(10 (5) (7)))                     ;; -> #t
(ordenado-tree? '(50 (10 (4) (6) (11)) (25) (80))) ;; -> #f
(ordenado-tree? '(1 (8) (7)))                      ;; -> #f
(ordenado-tree? '(50 (10 (4) (6) (8)) (-2 (15))))  ;; -> #f

;; Apartado b) (calcula-tree tree)

(define (operator op)    
  (cond     
    ((equal? op '+) +)   
    ((equal? op '-) -)   
    ((equal? op '*) *)  
    ((equal? op '/) /)
    (else (error "Operador desconocido: " op))))

(define (comprueba-lista lista)
  (if (null? (cdr lista))
     (list (calcula-tree (car lista)))
     (append (list (calcula-tree (car lista))) (comprueba-lista (cdr lista)))))

(define (calcula-tree tree)
  (if (hoja-tree? tree) (car tree)
     (if (aplica-f hoja-tree? (hijos-tree tree))   
        (apply (operator (dato-tree tree)) (hijos-dir tree))
        (apply (operator (dato-tree tree))(comprueba-lista (hijos-tree tree))))))


"Apartado b) (calcula-tree tree)"

(calcula-tree '(+ (- (5) (2)) (3))) ;; -> 6
(calcula-tree '(* (- (2) (+ (3) (* (4) (- (6) (2)) (3)) (1))) (1))) ;; -> -50
(calcula-tree '(- (8) (3) (9)))     ;; -> 4


;;
;; Ejercicio 4 
;;

;; Apartado a) (nivel-hoja dato lista)

(define (busca-nivel dato lista)
  (if (null? lista)
     #f
     (if (hoja?(car lista))
        (if (equal? dato (car lista))
           #t
           (busca-nivel dato (cdr lista))) 
        (busca-nivel dato (cdr lista)))))

(define (siguiente-nivel lista)
  (if (null? lista)'()
     (if (hoja? (car lista))
        (siguiente-nivel (cdr lista))
        (append (car lista) (siguiente-nivel (cdr lista))))))

(define (nivel-hoja dato lista)
  (if (hoja? lista)
      1
      (if (busca-nivel dato lista)
          1
          (+ 1 (nivel-hoja dato (siguiente-nivel lista))))))

"Pruebas apartado a) (nivel-hoja dato lista)"

(nivel-hoja 'b '(a b (c)))     ;; -> 1
(nivel-hoja 'b '(a (b) c))     ;; -> 2
(nivel-hoja 'b '(a c d ((b)))) ;; -> 3


;; Apartado b) (nivel-dato-tree dato tree)

(define (aplica-f-hijos dato lista)
  (if (null? (cdr lista))
     (nivel-dato-tree dato (car lista))
     (+ (nivel-dato-tree dato (car lista)) (aplica-f-hijos dato (cdr lista)))))

(define (nivel-dato-tree dato tree)
  (if (hoja-tree? tree)
     (if (equal? dato (dato-tree tree))
        0
        0)
     (if (busca-nivel dato tree)
        0
        (+ 1 (aplica-f-hijos dato (hijos-tree tree))))))

"Pruebas apartado b) (nivel-dato-tree dato tree)"

(nivel-dato-tree 30 '(20 (18) (19 (30) (32)) (4))) ;; -> 2
(nivel-dato-tree 20 '(20 (18) (19 (30) (32)) (4))) ;; -> 0
(nivel-dato-tree 5 '(5))                           ;; -> 0

;;
;; Ejercicio 5 (transformar plantilla lista)
;;

(define (dame-dato pos cont lista)
  (if (equal? pos cont)
     (car lista)
     (dame-dato pos (+ 1 cont) (cdr lista))))

(define (transformar plantilla lista)
  (if (null? plantilla)
      '()
      (if (hoja? (car plantilla))
        (if (null? (cdr plantilla))
           (list (dame-dato (car plantilla) 0 lista)) 
           (append (list (dame-dato (car plantilla) 0 lista)) (transformar (cdr plantilla) lista)))
        (append (list (transformar (car plantilla) lista))(transformar (cdr plantilla) lista)))))

"Pruebas ejercicio 5 (transformar plantilla lista)"

(transformar '((0 1) 4 (2 3)) '(hola que tal estas hoy)) ;; -> ((hola que) hoy (tal estas))
(transformar '(1 4 3 2 5 (0)) '(vamos todos a aprobar este examen)) ;; -> (todos este aprobar a examen (vamos))
(transformar '(2 0 3 1) '(me gusta mucho dormir))    ;; -> (mucho me dormir gusta)