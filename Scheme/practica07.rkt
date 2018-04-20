#lang r6rs
(import (rnrs)
        (schemeunit)
        (rnrs mutable-pairs))


;; Lawrence Rider García


;;
;; Practica 7
;;


;; Ejercicio 1
(define tree '(a (b (c (d)) (e)) (f)))

(define (hoja? dato)
  (not (list? dato)))
(define (dato-tree arbol) 
  (car arbol))
(define (hijos-tree arbol)
  (cdr arbol))
(define (hoja-tree? arbol)
  (null? (hijos-tree arbol)))

(define (to-string-tree tree)
  (if (null? tree)
      ""
      (string-append (symbol->string (dato-tree tree))
                     (to-list-bosque (hijos-tree tree)))))

(define (to-list-bosque bosque)
  (if (null? bosque)
      ""
      (string-append (to-string-tree (dato-tree bosque))
                     (to-list-bosque (hijos-tree bosque)))))


(define (to-string-tree-FOS tree)
  (string-append (symbol->string (dato-tree tree))
                 (fold-right string-append
                             ""
                             (map to-string-tree-FOS (hijos-tree tree)))))
;; Pruebas
(display "\nEjercicio1\n")
(display "(to-string-tree '(a (b (c (d)) (e)) (f))): ")
(display (to-string-tree '(a (b (c (d)) (e)) (f))))

(check-equal? (to-string-tree '(a (b (c (d)) (e)) (f))) "abcdef")
(check-equal? (to-string-tree '(o (p (q (r))) (s (t (u) (v))) (w))) "opqrstuvw")
(check-equal? (to-string-tree '(q (w) (e (r (t) (y))) (u) (i (o) (p)))) "qwertyuiop")


(display "\n(to-string-tree-FOS '(a (b (c (d)) (e)) (f))): ")
(display (to-string-tree-FOS '(a (b (c (d)) (e)) (f))))
(check-equal? (to-string-tree-FOS '(a (b (c (d)) (e)) (f))) "abcdef")
(check-equal? (to-string-tree-FOS '(o (p (q (r))) (s (t (u) (v))) (w))) "opqrstuvw")
(check-equal? (to-string-tree-FOS '(q (w) (e (r (t) (y))) (u) (i (o) (p)))) "qwertyuiop")


;; Ejercicio 4.a
(define x (cons (cons (cons 'a
                            (cons 'c 'vacio))
                      'e)
                (cons 'vacio 'd)))

(set-car! (cdr x) (cdr (car (car x))))
(set-cdr! (cdr (car (car x))) (cdr x))


;; Ejercicio 4.b
(set-cdr! (car x) (cdr x))
(set-car! (cdr x) x)
(set-car! (cdr (car (car x))) (cdr x))


;; Ejercicio 5.a
(define (rellena-lista! lista dato)
  (if (null? (cdr lista))
      (set-car! lista dato)
      (begin
        (set-car! lista dato)
        (rellena-lista! (cdr lista) dato))))

;; Pruebas
(display "\n\nEjercicio 5.a\n")
(display "(define lista '(1 2 3 4))\n(rellena-lista! lista 'a): ")

(define lista '(1 2 3 4))
(rellena-lista! lista 'a)

(display lista)

(check-equal? lista '(a a a a))

(define listax '(w 2 5 t 4 5 6 7))
(rellena-lista! listax #t)
(check-equal? listax (list #t #t #t #t #t #t #t #t))

(rellena-lista! lista '?)
(check-equal? lista '(? ? ? ?))


;; Ejercicio 5.b
(define (intercambia-listas! lista1 lista2)
  (if (or (null? (cdr lista1)) (null? (cdr lista2)))
      (let ((dato1 (car lista1)))
        (begin
          (set-car! lista1 (car lista2))
          (set-car! lista2 dato1)))
      (let ((dato1 (car lista1)))
        (begin
          (set-car! lista1 (car lista2))
          (set-car! lista2 dato1)
          (intercambia-listas! (cdr lista1) (cdr lista2))))))

;; Pruebas
(display "\n\nEjercicio 5.b\n")
(display "(define lista1 '(1 2 3 4 5))\n(define lista2 '(a b c))")
(display "\n(intercambia-listas! lista1 lista2)")

(define lista1 '(1 2 3 4 5))
(define lista2 '(a b c))
(intercambia-listas! lista1 lista2)

(display "\nlista1-> ")
(display lista1)
(display "\nlista2-> ")
(display lista2)


(check-equal? lista1 '(a b c 4 5))
(check-equal? lista2 '(1 2 3))

(define lista3 '(x y z 6))
(define lista4 '(a))
(intercambia-listas! lista3 lista4)
(check-equal? lista3 '(a y z 6))
(check-equal? lista4 '(x))

(define lista5 '(3 4 5))
(define lista6 '(q w e))
(intercambia-listas! lista5 lista6)
(check-equal? lista5 '(q w e))
(check-equal? lista6 '(3 4 5))

