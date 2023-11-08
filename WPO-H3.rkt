;;; WPO H3 ;;;

; 3.1.1 Recursief add

(define (rec-add a b)
  (if (= b 0)
      a
      (+ 1 (rec-add a (- b 1)))))

; 3.1.2 Iteratief add

(define (iter-add a b)
  (define (iter ctr res)
    (if (= ctr 0)
        res
        (iter (- ctr 1) (+ res 1))))
  (iter a b)) ;(iter b a) kan ook

;(4 5)
;(3 6)
;(2 7)
;(1 8)
;(0 9)

; 3.2 Multiply

(define (rec-multiply x y)
  (if (= y 0)
      0
      (+ x (rec-multiply x (- y 1)))))

(define (iter-multiply x y)
  (define (iter ctr res)
    (if (= ctr 0)
        res
        (iter (- ctr 1) (+ res x))))
  (iter y 0))

; 3.2.1 Fast Multiply

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (rec-fast-multiply x y)
  (cond
    ((= y 0) 0)
    ((even? y) (rec-fast-multiply (double x) (halve y)))
    (else (+ x (rec-fast-multiply x (- y 1))))))

(define (iter-fast-multiply x y) 
  (define (iter x y res)
    (if (= y 0)
        res
        (cond
          ((even? y) (iter (double x) (halve y) res))
          (else (iter x (- y 1) (+ res x))))))
  (iter x y 0))

;  3.3.1 Het getal e

;(define (factorial n)
;  (if (zero? n)
;      1
;      (* n (factorial (- n 1)))))
;
;(define (calc-e n)
;  (if (= n 0)
;      1
;      (+ (/ 1 (factorial n))
;         (calc-e (- n 1)))))

(define (calc-e n)
  (define (iter ctr prev-fac res)
    (if (> ctr n)
        (exact->inexact res) ;zodaht geen breuk is
        (iter (+ ctr 1) (* prev-fac ctr)
              (+ res (/ 1 (* prev-fac ctr)))))) ;..1/2! + 1/3!..
  (iter 1 1 1))

; 3.5 Mutuele recursie

(define (my-odd? x)
  (if (= x 0)
      #f
      (my-even? (- x 1))))

(define (my-even? x)
  (if (= x 0)
      #t
      (my-odd? (- x 1))))

;  3.6.1 Implementeer weird

(define (weird x)
  (cond
    ((= x 1) 1)
    ((even? x) (weird (/ x 2)))
    (else (weird (+ 1 (* 3 x))))))

; 3.6.2 Bereken de recursie diepte

(define (depth-weird x)
  (cond
    ((= x 1) 0)
    ((even? x) (+ 1 (depth-weird (/ x 2))))
    (else (+ 1 (depth-weird (+ 1 (* 3 x)))))))

; 3.7.1 Gebruik van de runtime-stack

(define (count1 x)
  (cond ((= 0 x) (display x))
        (else (display x)
              (count1 (- x 1)))))
;=> (count1 4) is 43210
(define (count2 x)
  (cond ((= 0 x) (display x))
        (else (count2 (- x 1)) ;runtime stack houdt alles bij
              (display x)))) ;dus zullen 1234 in stack zitten
;=> (count2 4) is 01234

; 3.7.2 Binaire vormen

(define (display-as-binary x)
  (if (> x 1)
      (display-as-binary (quotient x 2)))
  (display (modulo x 2)))

; 3.9.1 display-n

(define (display-n x n)
  (if (> n 0)
      (begin
        (display x)
        (display-n x (- n 1)))))

; 3.9.2 parasol

(define (parasol n)
  (define (teken-driehoek lijn-nr)
    (if (>= n lijn-nr)
        (begin ;bezig met sequenties, zodat if-tak
          (display-n " " (- n lijn-nr)) ;niet naar alternative gaat
          (display-n "*" (- (* 2 lijn-nr) 1))
          (newline)
          (teken-driehoek (+ lijn-nr 1)))))

  (define (teken-stok lijn-nr)
    (if (>= 3 lijn-nr)
        (begin
          (display-n " " (- n 1))
          (display-n "*" 1)
          (newline)
          (teken-stok (+ lijn-nr 1)))))

  (teken-driehoek 1)
  (teken-stok 1))