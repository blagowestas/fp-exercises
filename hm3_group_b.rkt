#lang racket
(require rackunit)
(require rackunit/text-ui)

;1.
(define (sum start end term next)
  (define (sum-i start result)
    (if (> start end) result
        (sum-i (next start) (+ (term start) result))))
  (sum-i start 0)
)

;(sum 1 100 (lambda (x) x) (lambda (x) (+ 1 x)))
;(sum 9 9 (lambda (x) x) (lambda (x) (+ 1 x)))

;2.
(define (accumulate-iter operation null-value start end term next)
  (define (acc-it start result)
    (if (> start end) result
        (acc-it (next start) (operation (term start) result))))
  (acc-it start null-value)
)

;(define (sum-using-acc start end)
;  (accumulate-iter + 0 start end (lambda (x) x) (lambda (x) (+ 1 x))))

;(sum-using-acc 1 100)

;3.
(define (accumulate-filter condition? operation null-value start end term next)
  (define (acc-filter start result)
    (cond ((> start end) result)
          ((condition? start) (acc-filter (next start) (operation (term start) result)))
          (else (acc-filter (next start) result))))
  (acc-filter start null-value)       
)

 (define (prime? x)
   (define (helper d)
     (cond
       ((> d (sqrt x)) #t)
       ((= (remainder x d) 0) #f)
       (else (helper (+ 1 d)))))
  (if (= x 1) #f (helper 2)))

(define (sum-prime start end)
  (accumulate-filter prime? + 0 start end (lambda (x) x) (lambda (x) (+ 1 x))))

(define (sum-even start end)
  (accumulate-filter even? + 0 start end (lambda (x) x) (lambda (x) (+ 1 x))))

;(sum-even 1 5)
;(sum-prime 1 100)

;4. липсва :D

;5.
(define (all-interval? start end next predicate?)
  (cond ((> start end) #t)
        ((not (predicate? start)) #f)
        (else (all-interval? (next start) end next predicate?)))     
)

;(all-interval? 2 9 (lambda (x) (+ x 2)) (lambda (x) (< x 10)))
;(all-interval? 3 13 (lambda (x) (+ x 2)) odd?)
;(all-interval? 3 11 (lambda (x) (+ x 2)) (lambda (x) (not (= (remainder x 3) 1))))

;6. + бонус - операцията е параметризирана
(define (combine-numbers first second g op)
  (define (reverse number result)
    (if (= (quotient number 10) 0)
        (+ (* result 10) number)
        (reverse (quotient number 10) (+ (* result 10) (remainder number 10))))
   )

  (define (combine num1 num2 result)
    (if (or (= (quotient num1 10) 0) (= (quotient num2 10) 0))
        (op result (g (remainder num1 10) (remainder num2 10)))
        (combine (quotient num1 10) (quotient num2 10) (op result (g (remainder num1 10) (remainder num2 10)))))
   )

  (combine (reverse first 0) (reverse second 0) 0)
)

;(combine-numbers 123 123 remainder +)
;(combine-numbers 421384 98 * +)
;(combine-numbers 12593 72397 (lambda (x y) (if (< x y) 1 0)) +)
;(combine-numbers 2713 98 (lambda (x y) (if (< x y) 1 0)) +)
;(combine-numbers 213 91423 (lambda (x y) (if (< x y) 1 0)) +)

;7.
(define (accumulate-rec op null-value start end term next)
  (if (> start end)
      null-value
      (op (term start) (accumulate-rec op null-value (next start) end term next))))

(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated f n)
  (if (= n 1) f
      (compose f (repeated f (- n 1)))))


(define (repeat-acc f n)
  (accumulate-iter compose f 1 (- n 1) (lambda (x) f) (lambda (x) (+ 1 x)))
)
 

(define tests
  (test-suite "Repeat tests"
      (check-equal? ((repeat-acc (lambda (x) (+ x 1)) 3) 5) 8)
      ; Искаме да проверим дали нашата accumulate версия прави същото като това, което написахме на упражнение
      (let ((f (lambda (x) (expt x 2)))
            (arg 2))
        (check-equal? ((repeat-acc f 2) arg) ((repeated f 2) arg)))
  )
)

(run-tests tests 'verbose)
