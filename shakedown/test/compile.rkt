#lang racket
(require "../compile.rkt"
         "../syntax.rkt"
         "../asm/interp.rkt"
         rackunit
         redex/reduction-semantics)

(define (run e)
  (asm-interp (compile (sexpr->prog e))))

(check-equal? (run 7) 7)
(check-equal? (run -8) -8)
(check-equal? (run '(add1 (add1 7))) 9)
(check-equal? (run '(add1 (sub1 7))) 7)

;; Examples from the notes
(check-equal? (run '(let ((x 7)) x)) 7)
(check-equal? (run '(let ((x 7)) 2)) 2)
(check-equal? (run '(let ((x 7)) (add1 x))) 8)
(check-equal? (run '(let ((x (add1 7))) x)) 8)
(check-equal? (run '(let ((x 7)) (let ((y 2)) x))) 7)
(check-equal? (run '(let ((x 7)) (let ((x 2)) x))) 2)
(check-equal? (run '(let ((x 7)) (let ((x (add1 x))) x))) 8)

; (check-equal? (run 'x) 'err)  ;; Not a valid program
(check-equal? (run '(add1 #f)) 'err)
(check-equal? (run '(+ 1 2)) 3)
(check-equal? (run '(zero? 0)) #t)
(check-equal? (run '(zero? 1)) #f)


;; Hustle tests
(check-equal? (run '(box 8)) (box 8))
(check-equal? (run '(unbox (box 8))) 8)
(check-equal? (run '(unbox 8)) 'err)

;; Iniquity tests
(check-equal? (run
               '(begin (define (f x) x)
                       (f 5)))
              5)
(check-equal? (run
               '(begin (define (tri x)
                         (if (zero? x)
                             0
                             (+ x (tri (sub1 x)))))
                       (tri 9)))
              45)
(check-equal? (run
               '(begin (define (even? x)
                         (if (zero? x)
                             #t
                             (odd? (sub1 x))))
                       (define (odd? x)
                         (if (zero? x)
                             #f
                             (even? (sub1 x))))
                       (even? 101)))
              #f)
(check-equal? (run
               '(begin (define (map-add1 xs)
                         (if (empty? xs)
                             '()
                             (cons (add1 (car xs))
                                   (map-add1 (cdr xs)))))
                       (map-add1 (cons 1 (cons 2 (cons 3 '()))))))
               '(2 3 4))
(check-equal? (run '(begin (define (f x) x)
                           f))
              'procedure)
(check-equal? (run '(begin (define (f x) x)
                           (f 5)))
              5)

;; Loot tests
(check-equal? (run '((λ (x) x) 7)) 7)
(check-equal? (run '(((λ (x) (λ (y) x)) 7) 8)) 7)
(check-equal? (run '((λ (f) (f 0)) (λ (x) (add1 x)))) 1)
(check-equal? (run '((λ (f) (f (f 0))) (λ (x) (add1 x)))) 2)
(check-equal? (run '((let ((y 8)) (car (cons (λ (x) y) '()))) 2)) 8)
(check-equal? (run '(let ((y 8)) ((car (cons (λ (x) y) '())) 2))) 8)
(check-equal?
 (run
  '(((λ (t)
       ((λ (f) (t (λ (z) ((f f) z))))
        (λ (f) (t (λ (z) ((f f) z))))))
     (λ (tri)
       (λ (n)
         (if (zero? n)
             1
             (+ n (tri (sub1 n)))))))
    10))
 56)

(check-equal?
 (run
  '(begin (define (map f ls)
            (if (empty? ls)
                '()
                (cons (f (car ls)) (map f (cdr ls)))))

          (map (λ (f) (f 0))
               (cons (λ (x) (add1 x))
                     (cons (λ (x) (sub1 x))
                           '())))))
 '(1 -1))

(check-equal?
 (run
  '(begin (define (map f ls)
            (letrec ((mapper (λ (ls)
                               (if (empty? ls)
                                   '()
                                   (cons (f (car ls)) (mapper (cdr ls)))))))
              (mapper ls)))
          (map (λ (f) (f 0))
               (cons (λ (x) (add1 x))
                     (cons (λ (x) (sub1 x))
                           '())))))
 '(1 -1))

;(check-equal?
; (run
;  '(begin (define (map f ls)
;            (begin (define (mapper ls)
;                     (if (empty? ls)
;                         '()
;                         (cons (f (car ls)) (mapper (cdr ls)))))
;                   (mapper ls)))
;          (map (λ (f) (f 0))
;               (cons (λ (x) (add1 x))
;                     (cons (λ (x) (sub1 x))
;                           '())))))
; '(1 -1))

(check-equal? (run
               '(let ((id (λ (x) x)))
                  (letrec ((even?
                            (λ (x)
                              (if (zero? x)
                                  #t
                                  (id (odd? (sub1 x))))))
                           (odd?
                            (λ (x)
                              (if (zero? x)
                                  #f
                                  (id (even? (sub1 x)))))))
                    (even? 101))))
              #f)

(check-equal? (run
               '(let ((id (λ (x) x)))
                  (id (letrec ((even?
                                (λ (x)
                                  (if (zero? x)
                                      #t
                                      (odd? (sub1 x)))))
                               (odd?
                                (λ (x)
                                  (if (zero? x)
                                      #f
                                      (even? (sub1 x))))))
                        (even? 101)))))
              #f)
