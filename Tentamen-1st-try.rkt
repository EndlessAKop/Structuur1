;;; TENTAMEN ;;;

;; 1 Recursie/Iteratie

(define (opbrengst-rec x c n)
  (if (= n 0)
      (exact->inexact x) ; zodat het geen breuk is
      (+ (opbrengst-rec x c (- n 1))
         (* (opbrengst-rec x c (- n 1))
            (/ c 100)))))

(define (opbrengst-iter x c n)
  (define (iter n res)
    (if (= n 0)
        (exact->inexact res)
        (iter (- n 1) (+ res (* res (/ c 100))))))
  (iter n x))

;; 2 Lijsten en Hogere Orde

(define (aantal-positief lst)
  (cond
    ((null? lst) 0)
    ((>= (car lst) 0) (+ 1 (aantal-positief (cdr lst))))
    (else (aantal-positief (cdr lst)))))

(define (aantal test lst)
  (cond
    ((null? lst) 0)
    ((test (car lst)) (+ 1 (aantal test (cdr lst))))
    (else (aantal test (cdr lst)))))

(define (aantal-pos lst)
  (aantal (lambda (x) (>= x 0)) lst))

;; 3 Lijsten

;(define (schrap-3 lst) DONT OVERTHINK BRUV
;  (define (hulp lst i res remove?)
;    (cond
;      ((null? lst) (reverse res))
;      ((= i 2) (hulp lst 0 res (not remove?)))
;      (remove? (hulp (cdr lst) (+ i 1) res remove?))
;      (else (hulp (cdr lst) (+ i 1) (cons (car lst) res) remove?))))
;  (hulp lst 0 '() #f))

(define (schrap-3 lst) ;=> juist
  (define (schrap lst i res)
    (cond
      ((null? lst) (reverse res))
      ((= i 2) (schrap (cdr lst) 0 res))
      (else (schrap (cdr lst) (+ i 1) (cons (car lst) res)))))
  (schrap lst 0 '()))

(define (schrap-n n lst) ;=> juist
  (define (schrap lst i res)
    (cond
      ((null? lst) (reverse res))
      ((= i (- n 1)) (schrap (cdr lst) 0 res))
      (else (schrap (cdr lst) (+ i 1) (cons (car lst) res)))))
  (schrap lst 0 '()))

;; 4 Recursie

;; a) zal een test uitvoeren op lijst
;; b) ja omdat test een functie is die je als argument opneemt en toepast

;; 5 Omgevingsmodel
; teken op blad

;; 6 Theorie

; a) zo dicht mogelijk tot een exact getal benaderen
; b) je hebt het absoluut waarde nodig hebt anders zal het getal negatief zijn
;    en dus een fout resultaat opleveren
; c) aan guess is het getal gebonden die we meegeven aan de hulpfunctie sqrt-iter
; d) ja het zal werken als normaal omdat het naam van parameter niet uitmaakt het
;    kon zegmaar ook "猜测" heten en zou evengoed werken