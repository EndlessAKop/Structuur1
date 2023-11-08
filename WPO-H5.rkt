;-------------------------------------Lijsten---------------------------------

;; Standaardpatroon voor recursieve, iteratieve procedures op lijsten

;(define (list-procedure-rec lst)
;  (if (null? lst)
;      base-result
;      (combine-car/res (do-something-with (car lst))
;                       (list-procedure-rec (cdr lst)))))


;(define (list-procedure-it lst)
;  (define (iter lst res)
;    (if (null? lst)
;        res
;        (iter (cdr lst)
;              (combine-car/res (do-something-with (car lst))
;                               res))))
;  (iter lst base-result))

;; voorbeeld:

(define (square x) (* x x))

(define (square-lst lst)
  (if (null? lst)
      '()
      (cons (square (car lst))
            (square-lst (cdr lst)))))

(define (square-lst-iter lst)
  (define (iter lst res)
    (if (null? lst)
        (reverse res)
        (iter (cdr lst)
              (cons (square (car lst))
                    res))))
  (iter lst '()))

(#%require racket/trace)

(define test (list 1 2 3 4))

(define (atom? x)
  (not (pair? x)))

;; 5.6 Element toevoegen achteraan een lijst

(define (add-to-end el lst)
  (if (null? lst)
      (cons el lst)
      (cons (car lst) ;je const car aan rest idiot
            (add-to-end el (cdr lst)))))

;; 5.7 Append (iteratief)

(define (myappend l1 l2) ;=> recursief
  (if (null? l1)
      l2
      (cons (car l1) ;cons neemt maar 2 args
            (myappend (cdr l1) l2))))

(define (myappend l1 l2) ;=> iteratief
  (define (iter lst res)
    (if (null? lst)
        res
        (iter (cdr lst)
              (cons (car lst) res))))
  (iter (reverse l1) l2))

;; 5.8 Lijsten reversen

(define (myreverse lst)
  (if (null? lst)
      '()
      (append (myreverse (cdr lst))
              (list (car lst))))) ;trukje om car achteraan te zetten

(define (iter-reverse lst)
  (define (iter lst res)
    (if (null? lst)
        res
        (iter (cdr lst)
              (cons (car lst) res))))
  (iter lst '()))

;; 5.9 Laatste element van lijst

(define (last lst)
  (cond
    ((null? lst) #f)
    ((null? (cdr lst)) (car lst))
    (else (last (cdr lst)))))

;; 5.10.1 Change

(define (change e1 e2 lst)
  (cond
    ((null? lst) lst)
    ((eq? (car lst) e1) (cons e2 (change e1 e2 (cdr lst))))
    (else (cons (car lst)
                (change e1 e2 (cdr lst))))))

;; 5.10.2 Change d.m.v. map

(define (change-dmv-map e1 e2 lst)
  (map (lambda (el)
         (if (eq? el e1)
             e2
             el))
       lst))

;; 5.11 My-equal?

(define (my-equal? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    ((eq? (car l1) (car l2)) (my-equal? (cdr l1) (cdr l2)))
    (else #f)))

;; 5.12.1 Sommatie van elementen in lijsten:
; Recursief

(define (rec-sum-lists l1 l2)
  (cond
    ((null? l1) l2)
    ((null? l2) l1)
    (else (cons (+ (car l1) (car l2))
                (rec-sum-lists (cdr l1) (cdr l2))))))

;Iteratief

;(define (iter-sum-lists l1 l2)
;  (define (iter l1 l2 res)
;    (cond ((and (null? l1) (null? l2)) (reverse res))
;          ((null? l1) (iter l1 (cdr l2) (cons (car l2) res)))
;          ((null? l2) (iter (cdr l1) l2 (cons (car l1) res)))
;          (else (iter (cdr l1)
;                      (cdr l2)
;                      (cons (+ (car l1) (car l2)) res)))))
;  (iter l1 l2 '()))
(define (iter-sum-lists l1 l2)
  (define (iter l1 l2 res)
    (cond
      ((null? l1) (append (reverse res) l2))
      ((null? l2) (append (reverse res) l1))
      (else (iter (cdr l1)
                  (cdr l2)
                  (cons (+ (car l1) (car l2))
                        res)))))
  (iter l1 l2 '()))

;; 5.15.1 Samenvoegen van twee lijsten

(define (rec-merge-n l1 l2 n)
  (define (merge l1 l2 i)
    (cond
      ((null? l1) l2)
      ((= i n) (merge l2 l1 0))
      (else (cons (car l1)
                  (merge (cdr l1) l2 (+ i 1))))))
  (merge l1 l2 0))

(define (iter-merge-n l1 l2 n)
  (define (iter-merge l1 l2 i res)
    (cond
      ((null? l1) (append (reverse res) l2))
      ((= i n) (iter-merge l2 l1 0 res))
      (else
       (iter-merge (cdr l1) l2 (+ i 1)
                   (cons (car l1) res)))))
  (iter-merge l1 l2 0 '()))

;; 5.15.2 Lijsten samenvoegen: Willekeurig aantal lijsten

(define (super-merge-n lsts n)
  (define (merge current rest i) ;current is huidige lijst die we aanht opeten zijn, i houdt bij hvl el van current zijn opgegeten, rest is gwn een lijst met alle andzre lijsten die we moetn opeten
    (cond
      ((and (null? current) (null? rest)) '())
      ((null? current) (merge (car rest) (cdr rest) 0))
      ((= i n) (merge (car rest) (append (cdr rest) (list current)) 0))
      (else (cons (car current)
                  (merge (cdr current) rest (+ i 1))))))
  (if (null? lsts)
      '()
      (merge (car lsts) (cdr lsts) 0)))

;-----------------------------------

;; 5.16 Examen Informatica eerste zit 1999

(define (rec-dissect-n lst n)
  (define (dissect lst i remove?)
    (cond
      ((null? lst) '())
      ((= i n) (dissect lst 0 (not remove?)))
      (remove? (dissect (cdr lst) (+ i 1) remove?))
      (else (cons (car lst)
                  (dissect (cdr lst) (+ i 1) remove?)))))
  (dissect lst 0 #f))

(define (iter-dissect-n lst n)
  (define (iter lst i remove? res)
    (cond
      ((null? lst) (reverse res))
      ((= i n) (iter lst 0 (not remove?) res))
      (remove? (iter (cdr lst)
                     (+ i 1)
                     remove?
                     res))
      (else (iter (cdr lst)
                  (+ i 1)
                  remove?
                  (cons (car lst) res)))))
  (iter lst 0 #f '()))

;; 5.17.1 Kopie van een lijst, zonder elementen in een interval

(define (all-but-interval lst van tot)
  (define (interval lst van tot)
    (cond
      ((null? lst) '())
      ((and (eq? (car lst) van) (eq? (car lst) tot))
       (cdr lst))
      ((> (car lst) tot) lst)
      ((< (car lst) van)
       (cons (car lst) (all-but-interval (cdr lst) van tot)))
      (else (all-but-interval (cdr lst) van tot))))
  (interval lst van tot))

(define (iter-all-but-interval lst van tot)
  (define (iter lst res)
    (cond
      ((null? lst) (reverse res))
      ((> (car lst) tot) (append (reverse res) lst))
      ((< (car lst) van) (iter (cdr lst) (cons (car lst) res)))
      (else (iter (cdr lst)
                  res))))
  (iter lst '()))

;; 5.18.1 Procedure p-taal

(define (klinker? x)
  (or (eq? x 'a) (eq? x 'e) (eq? x 'i)
      (eq? x 'o) (eq? x 'u)))

(define (rec-p-taal lst)
  (cond
    ((null? lst) lst)
    (else (append (if (klinker? (car lst)) ;kan met let ezer
                      (list (car lst) 'p (car lst))
                      (list (car lst)))
                  (rec-p-taal (cdr lst))))))

(define (iter-p-taal lst)
  (define (iter lst res)
    (if (null? lst)
        (reverse res)
        (iter (cdr lst)
              (append (if (klinker? (car lst))
                          (list (car lst) 'p (car lst))
                          (list (car lst)))
                      res))))
  (iter lst '()))

;; 5.19.1 Tel opeenvolging van letters

(define (rec-count-2-consecutive first second lst)
  (define (count prvs lst)
    (if (null? lst)
        0
        (let ((curr (car lst)))
          (+ (if (and (eq? prvs first)
                      (eq? curr second))
                 1
                 0)
             (count curr (cdr lst))))))
  (count (car lst) (cdr lst)))

(define (iter-count-2-consecutive first second lst)
  (define (iter prvs lst res)
    (if (null? lst)
        res
        (let ((curr (car lst))
              (rest (cdr lst)))
          (if (and (eq? prvs first)
                   (eq? curr second))
              (iter curr rest (+ res 1))
              (iter curr rest res)))))
  (iter (car lst) (cdr lst) 0))

;; 5.20.1 Splits resultaten van een examen

;; wtkk is deze oefening, but maken

;; 5.21.1 Vergelijken van twee lijsten

(define (rec-compare l1 l2)
  (cond
    ((and (null? l1) (null? l2)) 0)
    ((or (null? l1) (null? l2)) 0)
    ((eq? (car l1) (car l2)) (+ 1 (rec-compare (cdr l1) (cdr l2))))
    (else 0)))

(define (iter-compare l1 l2)
  (define (iter l1 l2 res)
    (cond
      ((and (null? l1) (null? l2)) res)
      ((or (null? l1) (null? l2)) res)
      ((eq? (car l1) (car l2)) (iter (cdr l1) (cdr l2) (+ res 1)))
      (else res)))
  (iter l1 l2 0))

;; 5.21.2 Vergelijk twee lijsten met een bepaalde test

(define (algemene-compare lijst1 lijst2 test)
  (cond
    ((and (null? l1) (null? l2)) 0)
    ((or (null? l1) (null? l2)) 0)
    ((test (car l1) (car l2)) (+ 1 (algemene-compare (cdr l1) (cdr l2) test)))
    (else 0)))

;; 5.22.1 Geslaagd

(define (rec-geslaagd namen punten)
  (cond
    ((null? namen) '())
    ((>= (car punten) 10) (cons (car namen) (rec-geslaagd (cdr namen) (cdr punten))))
    (else (rec-geslaagd (cdr namen) (cdr punten)))))

(define (iter-geslaagd namen punten)
  (define (iter namen punten res)
    (cond
      ((null? namen) (reverse res))
      ((>= (car punten) 10) (iter (cdr namen) (cdr punten)
                                  (cons (car namen) res)))
      (else (iter (cdr namen)
                  (cdr punten)
                  res))))
  (iter namen punten '()))

;; 5.24 Examen Informatica januari 2008

;; maken 