                                        ; Exercise 1.1

;; nah too easy

                                        ; Exercise 1.2


(/ (+ 4 5 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

                                        ; Exercise 1.3

(define (f a b c)
  (apply (lambda (x y) (+ (* x x) (* y y)))
         (cdr (sort (list a b c) <))))

                                        ; Exercise 1.4

;; The body of the function is a single expression of the form (<op> a b),
;; where <op> is chosen to be + if b is positive, and - if b is 0 or negative.

                                        ; Exercise 1.5

;; with applicative order, `(p)' is evaluated before `test' is invoked. Given
;; the definition of `p', this would result in an infinite loop (assuming the
;; interpreter is compliant with scheme in eliminating tail calls, otherwise
;; a stack overflow will occur).

;; with normal order, `(p)' is not evaluated, but rather `(test 0 (p))' is
;; expanded to `(if (= 0 0) 0 (p))' which is then evaluated to `0'.

;;;; code from the book ;;;;
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))
                                        ; Exercise 1.6

;; Due to the call-by-value nature of scheme, the `else' clause is always
;; evaluated with `new-if', and since the else clause is a tail-recursion, it
;; will simply loop forever.

                                        ; Exercise 1.7

;; `good-enough?' works by squaring the guess and deciding whether the result
;; is close enough to the value x for which `sqrt' is attempting to approximate
;; the square root. It deems a value close enough if it is within 0.001 of x.
;; Suppose x is 0.0001, and given that 0.001 is less than 0.001 away from
;; 0.0001, it would be possible for the value returned by (sqrt 0.0001) to
;; actually be closer to the square root of 0.001, which is an order of
;; magnitude larger. Indeed, invoking `sqrt' on any number between 0 and 0.001
;; could return the square root of any number between 0 and 0.001, depending
;; only on the guess-refine procedure

(good-enough? 0.001 0.0000000001) ;Value: #t

;; in addition, floating point numbers are inherently imprecise, which can cause
;; large numbers to be truncated such that numbers of a similar size can never
;; be determined to be within 0.001 of each other. This can cause an infinite
;; loop in `sqrt-iter'.

;(sqrt 4.2342342323423432e99) ; doesn't finish

;; so here is a version with the suggested improvements
(define (sqrt-iter2 guess x)
  (let ((new-guess  (improve guess x)))
    (if (< (/ (abs (- new-guess guess)) guess) 0.0001)
        new-guess
        (sqrt-iter2 new-guess x))))

(define (sqrt2 x)
  (sqrt-iter2 1 x))

;; note how this time it finishes executing!
(sqrt2 4.2342342323423432e99) ;Value: 6.507099379970976e49
(square 6.507099379970976e49) ;Value: 4.2342342340818654e99

;; and small values get much more accurate too.
(sqrt 1e-8)                    ;Value: .03125010656242753
(square .03125010656242753)    ;Value: 9.76569160163076e-4
(sqrt2 1e-8)                   ;Value: 1.0000000000082464e-4
(square 1.0000000000082464e-4) ;Value: 1.0000000000164927e-8


                                        ; Exercise 1.8

(define (cbrt-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cbrt-iter guess x)
  (let ((new-guess (cbrt-improve guess x)))
    (if (< (/ (abs (- new-guess guess)) guess) 0.001)￼
        new-guess
        (cbrt-iter new-guess x))))

(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 5) ;Value: 1.709975950782189

                                        ; Exercise 1.9
;; these don't seem to be defined
(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
9
;; recursive

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9
;; iterative


                                        ; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))


(A 1 10)
(A 0 (A 1 9))
(* 2 (A 0 (A 1 8)))
(* 2 (* 2 (A 0 (A 1 7))))
(* 2 (* 2 (* 2 (A 0 (A 1 6)))))
(* 2 (* 2 (* 2 (* 2 (A 0 (A 1 5))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 4)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 3))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 1))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 4))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 8)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 16))))))
(* 2 (* 2 (* 2 (* 2 (* 2 32)))))
(* 2 (* 2 (* 2 (* 2 64))))
(* 2 (* 2 (* 2 128)))
(* 2 (* 2 256))
(* 2 512)
1024

(A 2 4)
(A 1 (A 2 3))
(A 0 (A 1 (- (A 1 (A 2 2)) 1)))
(* 2 (A 0 (A 1 (- (- (A 0 (A 1 (- (A 1 (A 2 1)) 1))) 1) 1))))
(* 2 (* 2 (A 0 (A 1 (- (- (- (* 2 (A 0 (A 1 (- (- (A 0 (A 1 (- 2 1))) 1) 1)))) 1) 1) 1)))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 (* 2 (A 0 (A 1 (- (- (- (* 2 2) 1) 1) 1))))) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 (* 2 (A 0 (A 1 (- (- (- 4 1) 1) 1))))) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 (* 2 (A 0 (A 1 (- (- 3 1) 1))))) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 (* 2 (A 0 (A 1 (- 2 1))))) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 (* 2 (A 0 (A 1 1)))) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 (* 2 (A 0 2))) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 (* 2 (* 2 2))) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 (* 2 4)) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 8) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- 16 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- 15 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- 14 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- 13 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 12)))))
(* 2 (* 2 (* 2 (* 2 (A 0 (A 1 11))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 10)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 9))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 8)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 7))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 6)))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 5))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 4)))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 3))))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 2)))))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 1))))))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 4))))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 8)))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 16))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 32)))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 64))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 128)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 256))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 512)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 1024))))))
(* 2 (* 2 (* 2 (* 2 (* 2 2048)))))
(* 2 (* 2 (* 2 (* 2 4096))))
(* 2 (* 2 (* 2 8192)))
(* 2 (* 2 16384))
(* 2 32768)
65536


(A 3 3)
(A 2 (A 3 2))
(A 1 (A 2 (- (A 2 (A 3 1)) 1)))
(A 0 (A 1 (- (A 1 (A 2 (- (- (A 1 (A 2 1)) 1) 1))) 1)))
(A 0 (A 1 (- (A 1 (A 2 (- (- (A 1 2) 1) 1))) 1)))
(* 2 (A 0 (A 1 (- (- (A 0 (A 1 (- (A 1 (A 2 (- (- (- (A 0 (A 1 1)) 1) 1) 1))) 1))) 1) 1))))
(* 2 (A 0 (A 1 (- (- (A 0 (A 1 (- (A 1 (A 2 (- (- (- (A 0 2) 1) 1) 1))) 1))) 1) 1))))
(* 2 (A 0 (A 1 (- (- (A 0 (A 1 (- (A 1 (A 2 (- (- (- (* 2 2) 1) 1) 1))) 1))) 1) 1))))
(* 2 (A 0 (A 1 (- (- (A 0 (A 1 (- (A 1 (A 2 (- (- (- 4 1) 1) 1))) 1))) 1) 1))))
(* 2 (A 0 (A 1 (- (- (A 0 (A 1 (- (A 1 (A 2 (- (- 3 1) 1))) 1))) 1) 1))))
(* 2 (A 0 (A 1 (- (- (A 0 (A 1 (- (A 1 (A 2 (- 2 1))) 1))) 1) 1))))
(* 2 (A 0 (A 1 (- (- (A 0 (A 1 (- (A 1 (A 2 1)) 1))) 1) 1))))
(* 2 (A 0 (A 1 (- (- (A 0 (A 1 (- (A 1 2) 1))) 1) 1))))
(* 2 (* 2 (A 0 (A 1 (- (- (- (* 2 (A 0 (A 1 (- (- (A 0 (A 1 1)) 1) 1)))) 1) 1) 1)))))
(* 2 (* 2 (A 0 (A 1 (- (- (- (* 2 (A 0 (A 1 (- (- (A 0 2) 1) 1)))) 1) 1) 1)))))
(* 2 (* 2 (A 0 (A 1 (- (- (- (* 2 (A 0 (A 1 (- (- (* 2 2) 1) 1)))) 1) 1) 1)))))
(* 2 (* 2 (A 0 (A 1 (- (- (- (* 2 (A 0 (A 1 (- (- 4 1) 1)))) 1) 1) 1)))))
(* 2 (* 2 (A 0 (A 1 (- (- (- (* 2 (A 0 (A 1 (- 3 1)))) 1) 1) 1)))))
(* 2 (* 2 (A 0 (A 1 (- (- (- (* 2 (A 0 (A 1 2))) 1) 1) 1)))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 (* 2 (A 0 (A 1 1)))) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 (* 2 (A 0 2))) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 (* 2 (* 2 2))) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 (* 2 4)) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- (* 2 8) 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- (- 16 1) 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- (- 15 1) 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- (- 14 1) 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 (- 13 1))))))
(* 2 (* 2 (* 2 (A 0 (A 1 12)))))
(* 2 (* 2 (* 2 (* 2 (A 0 (A 1 11))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 10)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 9))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 8)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 7))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 6)))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 5))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 4)))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 3))))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 2)))))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 1))))))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 4))))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 8)))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 16))))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 32)))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 64))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 128)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 256))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 512)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 1024))))))
(* 2 (* 2 (* 2 (* 2 (* 2 2048)))))
(* 2 (* 2 (* 2 (* 2 4096))))
(* 2 (* 2 (* 2 8192)))
(* 2 (* 2 16384))
(* 2 32768)
65536


;; (f n) computes 2n
;; (g n) computes 2^n
;; (h n) computes 2^(2^n)
