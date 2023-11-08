;-------------------------------------TAAK 1---------------------------------

;; Golomb 
(define (golomb n)
  (if (= n 1)
      1
      (+ 1 (golomb (- n (golomb (golomb (- n 1))))))))

(define (golomb-reeks n)
  (if (= n 1)
      (display "1 ")
      (begin
        (golomb-reeks (- n 1))
        (display (golomb n))
        (display " "))))

;; Driehoek Pascal

(define (binom n k)
  (cond
    ((= k 0) 1)
    ((= k 1) n)
    ((> k n) 0)
    ((= n k) 1)
    (else (+ (binom (- n 1) (- k 1)) (binom (- n 1) k)))))
;; De binom procedure levert een recursief process.
;; Het eigenschap van een recursievf process is dat het lineair groeit tegenover grootte n.

(define (pascal-driehoek n)
  (define (print-rij rij kolom)
    (if (= rij kolom)
        (display 1)
        (begin
          (display (binom rij kolom))
          (display "\t")
          (print-rij rij (+ kolom 1)))))
  (define (print-driehoek rij)
    (if (= n rij)
        (print-rij n 0)
        (begin
          (print-rij rij 0)
          (newline)
          (print-driehoek (+ rij 1)))))
  (print-driehoek 0))