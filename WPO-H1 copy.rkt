;;; WPO H1 ;;;

;1)

;;; zie blad

; 2.1.1 Sign 1.3.1 fourth

(define (fourth-* x)
  (* x x x x))

(define (square x) (* x x))
(define (fourth-square x)
  (square (square x)))

; 1.3.2 sum-3-squares

(define (sum-3-squares x y z)
  (+ (square x) (square y) (square z)))

; 1.3.3 Celsius naar Fahrenheit

(define (convert-C-to-F x)
  (- (* (+ x 40) 1.8) 40))

; 1.8.1 Discount

(define (discount prijs korting)
  (- prijs (* (/ prijs 100.0) korting)))

;;; WPO H2 ;;;

; 2.1.1 Sign

(define (sign number)
  (cond
    ((= number 0) 0)
    ((> number 0) 1)
    ((< number 0) -1)))

; 2.1.2 leap-year?

(define (divides? x y)
  (= (modulo x y) 0))
(define (leap-year? x)
  (cond
    ((divides? x 400) #t)
    ((divides? x 100) #f)
    ((divides? x 4) #t)
    (else #f)))

; 2.3.1 my-and

(define (my-and x y)
  (if x
      y
      #f))

; 2.3.3 my-or

(define (my-or x y) ;als die iets ziet die #t is stopt die en kijkt die niet verder
  (if x
      #t
      y))

; 2.3.4 my-if

(define (my-if pred cons alt)
  (cond
    (pred cons)
    (else alt)))

; 2.3.5 check devision by zero

(define (check-db0-if x y)
  (if (= y 0)
      x
      (/ x y)))

(define (check-db0-my-if x y)
  (my-if (= y 0)
         x
         (/ x y)))

;> (check-db0-if 6 0)
;> (check-db0-my-if 6 0)

;==> nutteloos oefening