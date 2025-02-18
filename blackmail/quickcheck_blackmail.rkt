#lang racket

(require quickcheck
         rackunit
         rackunit/quickcheck)

(require "correct.rkt" "ast.rkt")
(require "interp.rkt")
(require "exec.rkt")

(define (gen n)
  (match n
    [0 (Lit (choose-integer 0 100))]
    [_ (choose-one-of (list
                       (Prim1 (choose-one-of '(add1 sub1)) (gen (- n 1)))
                       ))]))

(define (f a) (begin (printf "~a\n" a) #t))

(define (choose-prim [recurse-limit 3])
  (bind-generators
   ([rand (choose-integer 0 1)]
    [recurse? (and (positive? recurse-limit)
                   (= 0 rand))]
    [a (choose-one-of '(add1 sub1))]
    [b (if recurse?
           (choose-prim (sub1 recurse-limit))
           (bind-generators ([v (choose-integer 1 1000)]) (Lit v)))
       ]
           )
   (Prim1 a b)))

(with-test-count 10
(quickcheck
   (property ([e (choose-prim 15)])
             (let ((v (interp e)))
             (begin (printf "~a = ~a\n" e v)
              (eq? v (exec e)))
                    ;(eq? (void) (check-compiler e))
             ))))