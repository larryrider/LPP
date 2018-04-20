#lang r6rs
(import (rnrs)
        (schemeunit)
        (rnrs mutable-pairs))


;; Lawrence Rider García


;;
;; Practica 8
;;


;; Ejercicio 1
(define (ultima-pareja clista)
  (if (null? (cdr clista))
      clista
      (ultima-pareja (cdr clista))))

(define (crear-lista-circular! clista)
  (set-cdr! (ultima-pareja clista) (cdr clista)))


(define clista '(clista 1 4 9 7))
(define clista2 '(clista2 a b c))
(define clista3 '(clista3 1 g 2 j k))

(display "\n\nEjercicio 1\n")
(display "(define clista '(clista 1 4 9 7))\n(crear-lista-circular! clista)\nclista => ")
(crear-lista-circular! clista)
(display clista)
(display "\n\n(define clista2 '(clista2 a b c))\n(crear-lista-circular! clista2)\nclista2 => ")
(crear-lista-circular! clista2)
(display clista2)
(display "\n\n(define clista3 '(clista3 1 g 2 j k))\n(crear-lista-circular! clista3)\nclista3 => ")
(crear-lista-circular! clista3)
(display clista3)


;; Ejercicio 2
(define (intercambia-elementos! lista)
  (if (not (null? (cdr lista)))
      (let ((primero (cdr lista))
            (segundo (cddr lista))
            (tercero (cdddr lista)))
        
        (set-cdr! lista segundo)
        (set-cdr! (cdr lista) primero)
        (set-cdr! (cddr lista) tercero)
        
        (intercambia-elementos! (cddr lista)))))


(define lista '(clist 1 2 3 4))
(define lista2 '(clist2 a b c d))
(define lista3 '(clist3 1 g 2 j x do))

(display "\n\n\nEjercicio 2\n")
(display "(define lista '(clist 1 2 3 4))\n(intercambia-elementos! lista)\nlista => ")
(intercambia-elementos! lista)
(display lista)
(display "(\n\n(define lista2 '(clist2 1 2 3 4))\n(intercambia-elementos! lista2)\nlista2 => ")
(intercambia-elementos! lista2)
(display lista2)
(display "(\n\n(define lista3 '(clist3 1 2 3 4))\n(intercambia-elementos! lista3)\nlista3 => ")
(intercambia-elementos! lista3)
(display lista3)


;; Ejercicio 3
(define (make-pila)
  '(pila))

(define (push-pila! pila dato)
  (let ((resto (cdr pila)))
    (set-cdr! pila (list dato))
    (set-cdr! (cdr pila) resto)))

(define (pop-pila! pila)
  (if (not (null? (cdr pila)))
      (let ((elem (car (cdr pila))))
        (set-cdr! pila (cddr pila))
        elem)))

(define (vacia-pila? pila)
  (null? (cdr pila)))


(define pila1 (make-pila))
(display "\n\n\nEjercicio 3\n")
(display "(define pila1 (make-pila))\n(push-pila! pila1 10)")
(push-pila! pila1 10)
(display "\n(push-pila! pila1 \"hola\")")
(push-pila! pila1 "hola")
(display "\n(push-pila! pila1 'a)")
(push-pila! pila1 'a)
(display "\n(pop-pila! pila1) => ")
(display (pop-pila! pila1))
(display "\n(pop-pila! pila1) => ")
(display (pop-pila! pila1))
(display "\n(pop-pila! pila1) => ")
(display (pop-pila! pila1))
(display "\n(vacia-pila? pila1) => ")
(display (vacia-pila? pila1))
(display "\n(pop-pila! pila1) => ")
(pop-pila! pila1)



;; Ejercicio 4
(define (make-dic)
  (list '*dic*))

(define (get-dic dic clave)
  (let ((pareja (assq clave (cdr dic))))
    (if (not pareja)
        #f
        (car (cdr pareja)))))

(define (put-dic! dic clave valor)
  (let ((pareja (assq clave (cdr dic))))
    (if (not pareja)
        (set-cdr! dic (cons (cons clave (cons valor 1))
                            (cdr dic)))
        (set-cdr! pareja (cons valor (+ 1 (cdr (cdr pareja)))))))
    'ok)

(define (cambios-dic dic clave)
  (let ((pareja (assq clave (cdr dic))))
    (if (not pareja)
        0
        (cdr (cdr pareja)))))


(define dic (make-dic))
(display "\n\n\nEjercicio 4")
(display "\n(put-dic! dic 'a 20) => ")
(display (put-dic! dic 'a 20))
(display "\n(cambios-dic dic 'a) => ")
(display (cambios-dic dic 'a))
(display "\n(put-dic! dic 'a 30) => ")
(display (put-dic! dic 'a 30))
(display "\n(cambios-dic dic 'a) => ")
(display (cambios-dic dic 'a))
(display "\n(get-dic dic 'a) => ")
(display (get-dic dic 'a))
(display "\n(cambios-dic dic 'b) => ")
(display (cambios-dic dic 'b))
(display "\n(put-dic! dic 'b 10) => ")
(display (put-dic! dic 'b 10))
(display "\n(cambios-dic dic 'b) => ")
(display (cambios-dic dic 'b))


;; Ejercicio 5
(define (memorizador func)
  (let ((pila (make-pila)))
    (lambda (x) (if (equal? 'mem x)
                    (pop-pila! pila)
                    (begin
                      (push-pila! pila (func x))
                      (func x))))))

(display "\n\n\nEjercicio 5")
(define (cuadrado x)
 (* x x))
(define f (memorizador cuadrado))
(display "\n(f 10) => ")
(display (f 10))
(display "\n(f 3) => ")
(display (f 3))
(display "\n(f 2) => ")
(display (f 2))
(display "\n(f 'mem) => ")
(display (f 'mem))
(display "\n(f 'mem) => ")
(display (f 'mem))
(display "\n(f 6) => ")
(display (f 6))
(display "\n(f 'mem) => ")
(display (f 'mem))
(display "\n(f 'mem) => ")
(f 'mem)

