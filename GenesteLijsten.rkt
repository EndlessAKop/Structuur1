;;;--------------------------Geneste Lijsten------------------------;;;

; Model oplossing

(define (tree-procedure-rec lst)
  (cond ((null? lst) base-result)
        ((atom? lst) atom-result)
        (else (combine-branches (tree-procedure-rec (car lst))
                                (tree-procedure-rec (cdr lst))))))

(define (atom? x)
  (not (pair? x)))

(#%require racket/trace)

;; 7.2.1 Aantal elementen van een boom

(define (leaf-count tree)
  (cond
    ((null? tree) 0)
    ((atom? tree) 1)
    (else (+ (leaf-count (car tree))
             (leaf-count (cdr tree))))))

;; 7.2.2 Diepte van een boom

(define (depth tree)
  (cond
    ((null? tree) 0)
    ((atom? tree) 0)
    (else (max (+ 1 (depth (car tree))) ; max = maximum diepte (car deelboom)
               (depth (cdr tree))))))

;; 7.2.3 Diepte en aantal elementen van een boom

(define (depth-and-leaf-count tree)
  (let ((make-res cons))
    (cond
      ((null? tree) (make-res 0 0))
      ((atom? tree) (make-res 0 1))
      (else (let ((res-car (depth-and-leaf-count (car tree)))
                  (res-cdr (depth-and-leaf-count (cdr tree))))
              (make-res (max (+ 1 (car res-car))
                             (car res-cdr))
                        (+ (cdr res-car)
                           (cdr res-cdr))))))))

;; 7.3 fringe

(define (fringe lst)
  (cond
    ((null? lst) '())
    ((atom? lst) (list lst))
    (else (append (fringe (car lst))
                  (fringe (cdr lst)))))) ;list moet hier niet bij cuz je doet al bij atom

;; 7.4 unfringe

(define (unfringe lst)
  (cond
    ((null? lst) lst)
    ((null? (cdr lst)) (list (car lst)))
    (else (append (list (car lst))
                  (list (unfringe (cdr lst)))))))

;; 7.5 Structuur vergelijken

(define (same-structure? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    ((and (atom? l1) (atom? l2)) #t)
    ((or (atom? l1) (atom? l2)) #f)
    (else (and (same-structure? (car l1) (car l2))
               (same-structure? (cdr l1) (cdr l2))))))

;; 7.6.1 deep-combine

(define (deep-combine combiner null-value l)
  (cond
    ((null? l) null-value)
    ((atom? l) l) ;geen (combiner l) omdat het iets combineert, je vergist het met functie..
    (else (combiner (deep-combine combiner null-value (car l))
                    (deep-combine combiner null-value (cdr l))))))

;; 7.6.2 deep-map

(define (deep-map f l)
  (cond
    ((null? l) '())
    ((atom? l) (f l))
    (else (cons (deep-map f (car l))
                (deep-map f (cdr l))))))

;; 7.6.3 deep-change

(define (deep-change e1 e2 l)
  (deep-map (lambda (x) (if (eq? x e1)
                            e2
                            x))
            l))

;; 7.6.4 deep-atom-member?

(define (deep-atom-member? e l)
  (deep-combine (lambda (left right) (or left right))
                #f
                (deep-map (lambda (x) (eq? e x)) l)))

;; 7.6.5 count-atoms

(define (count-atoms l)
  (deep-combine + 0 (deep-map (lambda (x) 1) l)))

;; 7.7.1 tree-accumulate

(define (tree-accumulate tree term combiner null-value)
  (cond
    ((null? tree) null-value)
    ((atom? tree) (term tree)) ;; term tree want is term
    (else (combiner (tree-accumulate (car tree) term combiner null-value)
                    (tree-accumulate (cdr tree) term combiner null-value)))))

;--------------------------
;; 7.9 Examen Informatica Partieel januari 1995

(define boom
  '((blad (appel . golden))
    (blad (appel . granny))
    (((appel . golden) blad) blad (appel . cox))))
;-
(define (blad? boom)
  (eq? boom 'blad))

(define (appel? boom)
  (and (pair? boom) (eq? (car boom) 'appel)))

(define (leafs boom)
  (cond
    ((null? boom) 0)
    ((blad? boom) 1)
    ((appel? boom) 0) ; appel moet er ook direct bij anders error
    (else (+ (leafs (car boom))
             (leafs (cdr boom))))))
;-

(define (type apple) (cdr apple))

(define (all-apples boom)
  (cond
    ((null? boom) '())
    ((blad? boom) '())
    ((appel? boom) (list (type boom)))
    (else (append (all-apples (car boom))
                  (all-apples (cdr boom))))))

;-

(define (union l1 l2)
  (cond
    ((null? l1) l2)
    ((member (car l1) l2) (union (cdr l1) l2))
    (else (cons (car l1) (union (cdr l1) l2)))))

(define (apple-types boom)
  (cond
    ((null? boom) '())
    ((blad? boom) '())
    ((appel? boom) (list (type boom)))
    (else (union (apple-types (car boom))
                 (apple-types (cdr boom))))))

;-

(define (bewerk-boom boom doe-blad doe-appel combiner init)
  (cond
    ((null? boom) init)
    ((blad? boom) (doe-blad boom))
    ((appel? boom) (doe-appel boom))
    (else (combiner (bewerk-boom (car boom) doe-blad doe-appel combiner init)
                    (bewerk-boom (cdr boom) doe-blad doe-appel combiner init)))))

;-

(define (leafs-dmv-boom boom)
  (bewerk-boom boom (lambda (blad) 1) (lambda (appel) 0) + 0))

;-

(define (all-apples-dmv-bewerk boom)
  (bewerk-boom boom (lambda (blad) '()) (lambda (appel) (list (type appel))) append '()))
