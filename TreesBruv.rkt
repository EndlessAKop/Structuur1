(define family '(tom (jan) (mie (kris) (anja (ina))) (piet (bert) (frank))))

(define (parent family)
  (car family))
(define (subfams family)
  (cdr family))

(define (find? name family)
  (cond
    ((eq? name (parent family)) #t)
    (else (find-in name (subfams family)))))
(define (find-in name families)
  (cond
    ((null? families) #f)
    (else (or (find? name (car families))
              (find-in name (cdr families))))))

(define (offspring? n1 n2 family)
  (cond
    ((eq? n1 (parent family)) (find? n2 family))
    (else
     (offspring-in n1 n2 (subfams family)))))
(define (offspring-in n1 n2 families)
  (cond
    ((null? families) #f)
    (else (or (offspring? n1 n2 (car families))
              (offspring-in n1 n2 (cdr families))))))

(define (children name family)
  (cond
    ((eq? name (parent family))
     (map car (subfams family)))
    (else (children-in name (subfams family)))))
(define (children-in name families)
  (cond
    ((null? families) #f)
    (else (or (children name (car families))
              (children-in name (cdr families))))))
;;;;------------------------------------------
(define organigram
  '(directeur
    (hoofd-verkoop (verkoopsleider-vlaanderen)
                   (verkoopsleider-brussel))
    (hoofd-productie (hoofd-inkoop (bediende1)
                                   (bediende2)
                                   (bediende3))
                     (hoofd-fakturen))
    (hoofd-administratie (hoofd-personeel)
                         (hoofd-boekhouding))))

(define (baas org) (car org))
(define (sub org) (cdr org))

(define (bazen-van org p)
  
  (define (bazen-van org path)
    (cond
      ((eq? p (baas org)) path)
      (else
       (bazen-van-in (sub org) (cons (baas org) path)))))
  (define (bazen-van-in orgs path)
    (cond
      ((null? orgs) #f)
      (else
       (or (bazen-van (car orgs) path)
           (bazen-van-in (cdr orgs) path)))))

  (bazen-van org '()))

(define (hierarchisch? p1 p2 org)

  (define (hierarchisch? org path)
    (cond
      ((and (eq? p1 (baas org)) (member p2 path)) #t)
      ((and (eq? p2 (baas org)) (member p1 path)) #t)
      (else (hierarchisch-in (sub org)
                             (cons (baas org) path)))))
  (define (hierarchisch-in orgs path)
    (cond
      ((null? orgs) #f)
      (else
       (or (hierarchisch? (car orgs) path)
           (hierarchisch-in (cdr orgs) path)))))
  (hierarchisch? org '()))

(define (collegas p org)

  (define (collegas org path)
    (cond
      ((eq? p (baas org))
       (append path (werknemers-in (sub org))))
      (else (collegas-in (sub org) (cons (baas org) path)))))
  (define (collegas-in org path)
    (cond
      ((null? org) #f)
      (else
       (or (collegas (car org) path)
           (collegas-in (cdr org) path)))))
  (define (werknemers org)
    (cons (baas org) (werknemers-in (sub org))))
  (define (werknemers-in org)
    (cond
      ((null? org) '())
      (else (append (werknemers (car org))
                    (werknemers-in (cdr org))))))
  (collegas org '()))
;;;;------------------------------------------
(define VUBOrganigram
  '(VUB (academisch (rectoraat)
                    (faculteiten
                     (rechten (bachelor (ba-rechten)
                                        (ba-criminologie))
                              (master (ma-rechten)
                                      (ma-criminologie)))
                     (economie)
                     (wetenschappen (bachelor (ba-wiskunde)
                                              (ba-fysica)
                                              (ba-cw))
                                    (master (ma-wiskunde)
                                            (ma-fysica)
                                            (ma-cw)))))
        (administratief (personeel) (financien))))

(define (display-n n d)
  (cond ((> n 0) (display d)
                 (display-n (- n 1) d))))
 
(define (print-lijn aantalblanco tekst)
  (display-n aantalblanco " ")
  (display tekst)
  (newline))

(define (print-vanaf organigram label)
  (if (organigram-member organigram label)
      (print (organigram-member organigram label))))

(define (organigram-member organigram label)
  (if (eq? label (car organigram))
      organigram
      (organigram-member-in (cdr organigram) label)))

(define (organigram-member-in organigram label)
  (if (null? organigram)
      #f
      (or (organigram-member (car organigram) label)
          (organigram-member-in (cdr organigram) label))))

(define (print organigram)
  (define (hulp diepte organigram)
    (print-lijn diepte (car organigram))
    (for-each (lambda (organigram)
                (hulp (+ 1 diepte) organigram))
              (cdr organigram)))
  (hulp 0 organigram))

(define (print-tot organigram niveau)
  (define (print-tot organigram indent)
    (if (not (> indent niveau))
        (begin
          (print-lijn indent (car organigram))
          (print-tot-in (cdr organigram) (+ indent 1)))))
  
  (define (print-tot-in lst indent)
    (if (not (null? lst))
        (begin
          (print-tot (car lst) indent)
          (print-tot-in (cdr lst) indent))))

  (print-tot organigram 0))
;;;;------------------------------------------
(define familieboom '(jan (piet (frans (tom)
                                       (roel))
                                (mie))
                          (bram (inge (bert (ina)
                                            (ilse))
                                      (bart))
                                (iris))
                          (joost (els (ilse)))))

(define (vader i) (car i))
(define (kinderen i) (cdr i))
(define (laatste-nakomeling? i)
  (null? (kinderen i)))

(define (verdeel-democratisch fam waarde)

  (define (verdeel boom)
    (if (laatste-nakomeling? boom)
        1
        (+ 1 (verdeel-in (kinderen boom)))))
  (define (verdeel-in boom)
    (if (null? boom)
        0
        (+ (verdeel (car boom))
           (verdeel-in (cdr boom)))))
  (/ waarde (verdeel-in (kinderen fam))))

(define (budget fam list)

  (define (budget boom list)
    (if (null? list)
        0
        (+ (car list)
           (budget-in (kinderen boom) (cdr list)))))
  (define (budget-in boom list)
    (if (null? boom)
        0
        (+ (budget (car boom) list)
           (budget-in (cdr boom) list))))
  (budget-in (kinderen fam) list))

(define (verdeel boom waarde)
  (if (null? (kinderen boom))
      (list (list (vader boom) waarde))
      (verdeel-in (kinderen boom) waarde)))
(define (verdeel-in boom waarde)
  (let ((budget (if (null? boom) waarde (/ waarde (length boom)))))
  (if (null? boom)
      '()
      (append (verdeel (car boom) budget)
              (verdeel-in (cdr boom) (- waarde budget))))))
  ;;;;------------------------------------------