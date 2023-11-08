;;;-----------------------------HOGERE ORDE-----------------------------;;;

;; 4.1.1 Lambda

;(define opgave (let ((x 1)
;                     (y (+ 1 x)))
;                 (+ x y)))

  ;; 4.2.1 Lambda

  (define opgave (let* ((x 1)
                        (y (+ 1 x)))
                   (+ x y)))

  (define antw ((lambda (x)
                  ((lambda (y)
                     (+ x y))
                   (+ 1 x)))
                1))

;;----------------------------

  (define (sum term a next b) 
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))))

  ;; 4.7.1 Constructieve recursie

  (define (product factor a next b)
    (if (> a b)
        1
        (* (factor a) (product factor (next a) next b))))

  ;;  4.7.2 Staartrecursie

  (define (iter-product factor a next b)
    (define (iter a b res)
      (if (> a b)
          res
          (iter (next a)
                b
                (* (factor a) res))))
    (iter a b 1))

  ;; 4.7.3 Factorial

  (define (factorial n)
    (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

; 1(a)-->next-->2(..)-->next-->3(..) ... next-->b

  ;; 4.9.1 Accumulate

(define (accumulate combiner null-value term a next b) ;; => rec
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (accumulate-iter combiner null-value term a next b) ;; => iter
    (define (iter ctr res)
      (if (> ctr b)
          res
          (iter (next ctr) (combiner (term ctr) res))))
    (iter a null-value))

  ;; 4.9.2 Product en Sum

  (define (product factor a next b)
    (accumulate * 1 factor a next b))

  (define (sum term a next b)
    (accumulate + 0 term a next b))

  ;; 4.9.3 Add en Multiply

  (define (add a b)
    (accumulate + a (lambda (x) 1) 1 (lambda (x) (+ x 1)) b))

  (define (multiply a b)
    (accumulate + 0 (lambda (x) a) 1 (lambda (x) (+ x 1)) b))

  ;; 4.10.1 Filtered-accumulate

(define (filtered-accumulatee combiner filter? null-value term a next b) ;; => rec
  (if (> a b)
      null-value
      (if (filter? a)
          (combiner (term a) (filtered-accumulatee combiner filter? null-value term (next a) next b))
          (filtered-accumulatee combiner filter? null-value term (next a) next b))))
(define (filtered-accumulate combiner filter? null-value term a next b) ;; => iter
    (define (iter ctr res)
      (if (> ctr b)
          res
          (iter (next ctr)
                (if (filter? ctr)
                    (combiner (term ctr) res)
                    res))))
    (iter a null-value))

  ;; 4.10.2 Grootste gemene deler

  (define (product-gcd n)
    (define (filter? i)
      (= 1 (gcd i n)))
    (filtered-accumulate * filter? 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))
  