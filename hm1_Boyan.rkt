; firts exercise - Boyan

#lang racket
(require racket/trace)

; 1.
(define a (+ (/ (+ 2 (/ 3 16)) (* 9 2.78)) (- (/ 5 2) 6)))

(define b (/ (+ 15 21 (/ 3 15) (- 7 (* 2 2))) 16))

(define c (/ (+ 5 1/4 (- 2 (- 3 (+ 6 1/5)))) (* 3 (- 6 2) (- 2 7))))

(define (square x) (* x x))

(define d (/ (+ (square 3) 5) (- (* (square 3) 3) 2)))

(define e (+ (* (square 16) (square 16)) 95/2))

; 2.
(define (square-sum-bigger-two a b c)
  (cond
    ((and (> a c) (> b c)) (+ (square a) (square b)))
    ((and (> a b) (> c b)) (+ (square a) (square c)))
    ((and (> b a) (> c a)) (+ (square b) (square c)))
    (else (display "We have equal numbers."))
    )  
)

; 3. 
(define (sum-interval start end)
  (if (> start end)
       0
       (+ start (sum-interval (+ 1 start) end)))
)

(define (sum-interval-q start end result)
  (if (> start end)
       result
       (sum-interval-q (+ 1 start) end (+ start result)))
)

;4
(define (count-digits number)
  (if (= (quotient number 10) 0) 1
      (+ 1(count-digits (quotient number 10))))
)

(define (count-digits-q number result)
  (if (= (quotient number 10) 0) (+ 1 result)
      (count-digits-q (quotient number 10) (+ 1 result)))
)

;5
(define (reverse-digits number)
  (define (helper number result)
    (if (not(= number 0)) (helper (quotient number 10) (+ (* 10 result) (remainder number 10)))
        result))
  (helper number 0)
 )

;6
(define (palindrome? number)
  (= (reverse-digits number) number))

(trace count-digits)

