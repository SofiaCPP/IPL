#lang racket

;hiiii
#t

(define (succ x) (+ x 1))

(define (pred x) (- x 1))

(define (safe-div x)
  (if (even? x)
    (/ x 2)
    x))

(define (month-index x)
  (cond [(equal? x "January") 1]
        [(equal? x "February") 2]
        [(equal? x "March") 3]
        [(equal? x "April") 4]
        [(equal? x "May") 5]
        [(equal? x "June") 6]
        [(equal? x "July") 7]
        [(equal? x "August") 8]
        [(equal? x "September") 9]
        [(equal? x "October") 10]
        [(equal? x "November") 11]
        [(equal? x "December") 12]
        [else "Error"]))

(define (is-root? x)
  (or  (= x 1)
       (= x (/ -1 3))))

(define (factorial x)
  (if (= x 1) 1 (* x (factorial(pred x)))))

(define (fibonacci x)
  (cond [(= x 1) 1]
        [(= x 2) 1]
        [else (+ (fibonacci(pred x)) (fibonacci(pred(pred x))))]))

(define (add x y)
  (if (= y 0) x
      (add (succ x) (pred y))))

(define (multiply x y)
  (if (= y 0) 0
      (add x (multiply x (pred y)))))

'(1 . (2 . 3))