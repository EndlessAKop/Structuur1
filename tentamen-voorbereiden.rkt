(define (tip bedrag)
  (- (ceiling (+ bedrag (* 0.15 bedrag)))bedrag))

;;-------------------

(define (rec-add a b)
  (if (= b 0)
      a
      (+ 1 (rec-add a (- b 1)))))

(define (iter-add a b)
  (define (iter ctr res)
    (if (= ctr 0)
        res
        (iter (- ctr 1) (+ res 1))))
  (iter b a))

(define (rec-multiply a b)
  (if (= b 0)
      0
      (+ a (rec-multiply a (- b 1)))))

(define (iter-multiply a b)
  (define (iter a b res)
    (if (= b 0)
        res
        (iter a (- b 1) (+ a res))))
  (iter a b 0))

(define (rec-fast-multiply a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond
    ((= b 0) 0)
    ((even? b) (rec-fast-multiply (double a) (halve b)))
    (else (+ a (rec-fast-multiply a (- b 1))))))

(define (iter-fast-multiply a b)
  (define (iter a b res)
    (define (double x) (+ x x))
    (define (halve x) (/ x 2))
    (cond
      ((= b 0) res)
      ((even? b) (iter (double a) (halve b) res))
      (else (iter a (- b 1) (+ a res)))))
  (iter a b 0))

(define (calc-e n)
  (define (hulp ctr previous res)
    (if (> ctr n)
        (exact->inexact res)
        (let ((new-fac (* ctr previous)))
          (hulp (+ ctr 1) new-fac (+ res (/ 1 new-fac))))))
  (hulp 1 1 1))

(define (calc-sin x n)
  (define (hulp ctr res fac xpow sign)
    (if (> ctr n)
        res
        (let* ((i (- (* 2 ctr) 1))
               (newfac (* fac (- i 1) i))
               (newxpow (* xpow x x))
               (newsign (- sign)))
          (iter (+ ctr 1)
                (+ res (/ (* newsign newxpow) newfac))
                newfac
                newxpow
                newsign))))
  (hulp 2 x 1 x 1))

;;-----------------------

(define (add-to-end el lst)
  (if (null? lst)
      (cons el lst)
      (cons (car lst)
            (add-to-end el (cdr lst)))))

(define (apend l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (apend (cdr l1) l2))))
(define (attend l1 l2)
  (define (iter lst res)
    (if (null? lst)
        res
        (iter (cdr lst) (cons (car lst) res))))
  (iter (reverse l1) l2))

(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst))
              (list (car lst)))))
(define (iter-reverse lst)
  (define (iter lst res)
    (if (null? lst)
        res
        (iter (cdr lst) (cons (car lst) res))))
  (iter lst '()))

(define (last lst)
  (cond
    ((null? lst) lst)
    ((null? (cdr lst)) (car lst))
    (else (last (cdr lst)))))

(define (change e1 e2 lst)
  (cond
    ((null? lst) '())
    ((eq? (car lst) e1) (cons e2 (change e1 e2 (cdr lst))))
    (else (cons (car lst)
                (change e1 e2 (cdr lst))))))

(define (change-dmv-map e1 e2 lst)
  (map (lambda (x) (if (eq? x e1)
                       e2
                       x))
       lst))

(define (my-equal? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    ((eq? (car l1) (car l2)) (my-equal? (cdr l1) (cdr l2)))
    (else #f)))

(define (rec-sum-lists l1 l2)
  (cond
    ((null? l1) l2)
    ((null? l2) l1)
    (else (cons (+ (car l1) (car l2))
                (rec-sum-lists (cdr l1) (cdr l2))))))
(define (iter-sum-lists l1 l2)
  (define (iter l1 l2 res)
    (cond
      ((null? l1) (append (reverse res) l2))
      ((null? l2) (append (reverse res) l1))
      (else (iter (cdr l1) (cdr l2)
                  (cons (+ (car l1) (car l2)) res)))))
  (iter l1 l2 '()))

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
      (else (iter-merge (cdr l1) l2 (+ i 1)
                        (cons (car l1) res)))))
  (iter-merge l1 l2 0 '()))

(define (super-merge-n lsts n)
  (define (merge curr rest i)
    (cond
      ((and (null? curr) (null? rest)) '())
      ((null? curr) (merge (car rest) (cdr rest) 0))
      ((= i n) (merge (car rest)
                      (append (cdr rest)
                              (list curr))
                      0))
      (else (cons (car curr)
                  (merge (cdr curr) rest (+ i 1))))))
  (if (null? lsts)
      '()
      (merge (car lsts) (cdr lsts) 0)))

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
  (define (dissect lst i res remove?)
    (cond
      ((null? lst) (reverse res))
      ((= i n) (dissect lst 0 res (not remove?)))
      (remove? (dissect (cdr lst) (+ i 1) res remove?))
      (else (dissect (cdr lst) (+ i 1) (cons (car lst) res) remove?))))
  (dissect lst 0 '() #f))

(define (split lst n)
  (define (splitter lst i split? resA resB)
    (cond
      ((null? lst) (list (reverse resA)
                         (reverse resB)))
      ((= i n) (splitter lst 0 (not split?) resA resB))
      (split? (splitter (cdr lst)
                        (+ i 1)
                        split?
                        resA
                        (cons (car lst) resB)))
      (else (splitter (cdr lst) (+ i 1) split? (cons (car lst) resA) resB))))
  (splitter lst 0 #f '() '()))

(define (all-but-interval lst start stop)
  (cond
    ((null? lst) '())
    ((> (car lst) stop) lst)
    ((< (car lst) start)
     (cons (car lst)
           (all-but-interval (cdr lst) start stop)))
    (else (all-but-interval (cdr lst) start stop))))
(define (iter-all-but-interval lst start stop)
  (define (iter lst res)
    (cond
      ((null? lst) (reverse res))
      ((> (car lst) stop) (append (reverse res) lst))
      ((< (car lst) start) (iter (cdr lst) (cons (car lst) res)))
      (else (iter (cdr lst) res))))
  (iter lst '()))

(define (klinker? x)
  (or (eq? x 'a) (eq? x 'e) (eq? x 'i)
      (eq? x 'o) (eq? x 'u)))

(define (rec-p-taal lst)
  (if (null? lst)
      '()
      (append (let ((letter (car lst)))
                (if (klinker? letter)
                    (list letter 'p letter)
                    (list letter)))
              (rec-p-taal (cdr lst)))))
(define (iter-p-taal lst)
  (define (iter lst res)
    (if (null? lst)
        (reverse res)
        (iter (cdr lst)
              (append (let ((letter (car lst)))
                        (if (klinker? letter)
                            (list letter 'p letter)
                            (list letter)))
                      res))))
  (iter lst '()))

(define (rec-count-2-consecutive first second lst)
  (define (count prev lst)
    (if (null? lst)
        0
        (let ((curr (car lst)))
          (+ (if (and (eq? prev first)
                      (eq? second curr))
                 1
                 0)
             (count curr (cdr lst))))))
  (if (null? lst)
      0
      (count (car lst) (cdr lst))))

(define (iter-count-2-consecutive first second lst)
  (define (iter-count prev lst res)
    (if (null? lst)
        res
        (let ((curr (car lst))
              (rest (cdr lst)))
          (if (and (eq? prev first)
                   (eq? second curr))
              (iter-count curr rest (+ res 1))
              (iter-count curr rest res)))))
  (if (null? lst)
      0
      (iter-count (car lst) (cdr lst) 0)))