(define (cddr s)
  (cdr (cdr s)))

(define (cadr s)
  (car (cdr s))
)

(define (caddr s)
  (car (cddr s))
)


(define (sign num)
  (cond
    ((> num 0) 1)
    ((= num 0) 0)
    (else -1)
  )
)


(define (square x) (* x x))

(define (pow x y)
  (if (= y 0)
    1
    (if (even? y)
      (square (pow x (/ y 2)))
      (* x (square (pow x (/ (- y 1) 2))))
    )
  )
)


(define (unique s)
  (if (equal? s nil)
    nil
    (cons
      (car s)
      (unique
        (filter
          (lambda
            (x)
            (not (eq? x (car s)))
          )
          (cdr s)
        )
      )
    )
  )
)


(define (replicate x n)
  (define (itercate lst count)
    (if (> count n)
      lst
      (itercate
        (append lst (list x))
        (+ count 1)
      )
    )
  )
  (itercate nil 1)
)


(define (accumulate combiner start n term)
  (if (= n 0)
    start
    (combiner
      (term n)
      (accumulate
        combiner
        start
        (- n 1)
        term
      )
    )
  )
)


(define (accumulate-tail combiner start n term)
  (define (iter-accumulate combiner term sum count)
    (if (> count n)
      (combiner start sum)
      (iter-accumulate
        combiner
        term
        (combiner sum (term count))
        (+ count 1)
      )
    )
  )
  (iter-accumulate combiner term (term 1) 2)
)


(define-macro (list-of map-expr for var in lst if filter-expr)
  `(map (lambda (,var) ,map-expr) (filter (lambda (,var) ,filter-expr) ,lst))
)

