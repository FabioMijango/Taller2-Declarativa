#lang racket

; Ejercicio 1
(define (count-positives lst)
  (length (filter (lambda (x) (> x 0)) lst)))

(define ej1 '(3 -2 7 0 -5 9))
(displayln "Ejercicio 1:")
(displayln (count-positives ej1))

; Ejercicio 2
(define (square_pair lst)
   (map (lambda (x) (* x x)) (filter even? lst)))

(define ej2 '(1 2 3 4 5 6 7 8))
(displayln "Ejercicio 2:")
(displayln (square_pair ej2))

; Ejercicio 3
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(displayln "Ejercicio 3:")
(displayln (factorial 5))

; Ejercicio 4
(define (cube lst)
  (map (lambda (x) (* x x x)) lst))

(define ej4 '(1 2 3 4 5))
(displayln "Ejercicio 4:")
(displayln (cube ej4))


; Ejercicio 5
(define (sum_odd lst)
   (foldl + 0 (filter odd? lst)))

(define ej5 '(1 2 3 4 5 6 7))
(displayln "Ejercicio 5:")
(displayln (sum_odd ej5))


; Ejercicio 6
(define (has_negative lst)
  (ormap (lambda (x) (< x 0)) lst))

(define ej6 '(5 9 -3 2))
(displayln "Ejercicio 6:")
(displayln (has_negative ej6))


; Ejercicio 7
(define (cumulative_sum lista)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lista)))

(define ej7 '(1 2 3 4))
(displayln "Ejercicio 7:")
(displayln (cumulative_sum ej7))


; Ejercicio 8
(define (concat_strings str-list)
  (foldl string-append "" str-list))

(define ej8 '("Hello, " "world!" " How " "are " "you?"))
(displayln "Ejercicio 8:")
(displayln (concat_strings ej8))

; Ejercicio 9
(define (double_greater_than_five lst)
  (map (lambda (x) (* 2 x)) (filter (lambda (x) (> x 5)) lst)))

(define ej9 '(3 6 8 2 10))
(displayln "Ejercicio 9:")
(displayln (double_greater_than_five ej9))


; Ejercicio 10
(define (reverse_list lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))

(define ej10 '(1 2 3 4))
(displayln "Ejercicio 10:")
(displayln (reverse_list ej10))


; Ejercicio 11
(define (funct_as_param f lst)
  (map f lst))

(define ej11 '(1 2 3 4))
(define (square x) (* x x))
(displayln "Ejercicio 11:")
(displayln (funct_as_param square ej11))


; Ejercicio 12
(define (promedio-mayores-a-5 lista)
  (let* ((mayores (filter (lambda (x) (> x 5)) lista))
         (suma (foldl + 0 mayores))
         (cantidad (length mayores)))
    (if (> cantidad 0)
        (/ suma cantidad)
        0))) ;; en caso de que no haya números > 5

(define ej12 '(3 8 10 4 9 2 7))
(displayln "Ejercicio 12:")
(displayln (promedio-mayores-a-5 ej12))