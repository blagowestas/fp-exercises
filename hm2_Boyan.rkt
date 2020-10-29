#lang racket
;1 - въпрос

;2
(define (prime? number)
  (define (helper number devisor)
    (cond
      ((= number 1) #f)
      ((> (* devisor devisor) number) #t)
      ((= (modulo number devisor) 0) #f)
      (else (helper number (+ 1 devisor)))))
 (helper number 2)
 )
       
;3.
(define (to-binary number)
  (define (reverse-digits number result)
    (if (= number 0) result
        (reverse-digits (quotient number 10) (+ (* 10 result) (remainder number 10)))
        ))

  
  (define (helper number result)
    (cond ((< number 2) (reverse-digits (+ (* 10 result) 1) 0))
        ((= (remainder number 2) 0) (helper (quotient number 2) (* result 10)))
        (else  (helper (quotient number 2) (+ (* 10 result) 1)))))

 (quotient (helper number 1) 10)
)

;4.
(define (to-decimal number)
  (define (pow x a result)
    (if (= a 0) result
        (pow x (- a 1) (* result x))))
  
  (define (helper number power result)
    (if (= number 0) result
        (helper (quotient number 10) (+ 1 power) (+ result (* (remainder number 10) (pow 2 power 1))) )))

  (helper number 0 0)
)


;5.
(define (sum-digits number)
  (define (helper number result)
    (if (= number 0) result
        (helper (quotient number 10) (+ result (remainder number 10)))))
  (helper number 0)
)
        
;6.
(define (expt x n)
  (define (helper x pow result)
    (if (= pow 0) result
        (helper x (- pow 1) (* x result))))

  (if (= (remainder n 2) 0) (helper (* x x) (/ n 2) 1)
      (helper (* x x) (/ (- n 1) 2) x)) 
)

;7.
(define (ends-with? number test)
  (cond ((= (quotient test 10) 0) (= test (remainder number 10)))
        ((= (remainder number 10) (remainder test 10)) (ends-with? (quotient number 10) (quotient test 10)))
        (else #f)))

