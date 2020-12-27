;; ('fail, 'abs, 'str, 'id)
(define fail 'fail)
(define absent 'absent)
(define strict 'strict)
(define id 'id)
;; can check <= between elements of my domain
(define (flat-leq x y)
  (cond
    ((equal? x y) #t)
    ((equal? x fail) #t)
    ((and (equal? x strict) (equal? y id)) #t)
    ((and (equal? x absent) (equal? y id)) #t)
    (else #f)))

;; x U y
(define (flat-union x y)
  (cond
   ((equal? (flat-leq x y) x) x)
   ((equal? (flat-leq x y) y) y)
   (else id)))


(define (k x y) x)
