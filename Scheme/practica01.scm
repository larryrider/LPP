;; Lawrence Rider Garcia 

;; Practica 01

;; 
;; Ejercicio 1: lista
;;

(define lista1 '(4 2 3 (6 5 1) (9) (7(10(11 13(14))))))

"Pruebas ejercicio 1"
(car(cdr(cdr(cdr (cdr lista1))))) ;; => (9)

(car(cdr(cdr(cdr(cdr (cdr lista1)))))) ;; => (7 (10 (11 13 (14))))

(car(cdr(car(cdr(cdr (cdr lista1)))))) ;; => 5

(car(car(cdr(car(cdr(cdr(cdr(cdr (cdr lista1))))))))) ;; => 10


;; 
;; Ejercicio 2: mayor de los tres
;;

(define (mayor-de-tres n1 n2 n3)
  (if (>= n1 n2)
      (if (> n1 n3)
          n1
          n3)
      (if (>= n2 n3)
          n2
          n3)))
      
          
"Pruebas ejercicio 2"
  
(mayor-de-tres 2 2 2) ;; => 2
(mayor-de-tres 1 6 6) ;; => 6
(mayor-de-tres 5 4 3) ;; => 5
(mayor-de-tres 5 8 4) ;; => 8
(mayor-de-tres 5 4 9) ;; => 9

;; 
;; Ejercicio 3: engloba
;;

(define (engloba? a1 a2 b1 b2)
  (if (or (and (<= a1 b1) (<= b2 a2)) (and (<= b1 a1) (<= a2 b2)))
         #t
         #f))

"Pruebas ejercicio 3"

(engloba? 2 3 6 10)  ;; => #f
(engloba? 1 4 3 9)   ;; => #f
(engloba? 5 12 7 8)  ;; => #t
(engloba? 7 8 5 12)  ;; => #t

;; 
;; Ejercicio 4: interseccion
;;

(define (intersectan? a1 a2 b1 b2) 
  (and (<= b1 a2) (<= a1 b2)))

(define (intersectar a1 a2 b1 b2)
  (cons (max a1 b1) (min a2 b2)))
          
(define (interseccion a1 a2 b1 b2)
  (if(intersectan? a1 a2 b1 b2)
     (intersectar a1 a2 b1 b2)
     '()))

"Pruebas ejercicio 4"   


(interseccion 5 12 7 8) ;; => (7 . 8)
(interseccion 5 12 4 8) ;; => (5 . 8)
(interseccion 4 8 5 12) ;; => (5 . 8)
(interseccion 3 5 4 6)  ;; => (4 . 5)
(interseccion 4 8 9 11) ;; => ()
(interseccion 8 9 3 4)  ;; => ()



;;
;; Ejercicio 5: convertidor rgb->cmyk
;;

(define (rgb->cmyk r g b)
  (if (and (= r 0) (= g 0) (= b 0))
          '(0 0 0 1)
          (list (calcular-color (blanco r g b) r) 
                (calcular-color (blanco r g b) g)
                (calcular-color (blanco r g b) b)
                (- 1 (blanco r g b)))))

(define (blanco r g b)
  (/ (mayor-de-tres r g b) 255))

(define (calcular-color blanco color)
  (/ (- blanco (/ color 255)) blanco))


"Pruebas ejercicio 5"
(rgb->cmyk 0 0 0)       ;; => (0 0 0 1)
(rgb->cmyk 255 255 255) ;; => (0 0 0 0)
(rgb->cmyk 75 0 130)    ;; => (11/26 1 0 25/51)
(rgb->cmyk 150 10 255)  ;; => (7/17 49/51 0 0)
(rgb->cmyk 48 77 156)   ;; => (9/13 79/156 0 33/85)
