#lang racket
(require "../compile.rkt"
         "../interp.rkt"
         "../interp-io.rkt"
         "../asm/interp.rkt"
         "../parse.rkt"
         rackunit)

(define (test-runner run)
  ;; Abscond examples
  (check-equal? (run 7) 7)
  (check-equal? (run -8) -8)

  ;; Blackmail examples
  (check-equal? (run '(add1 (add1 7))) 9)
  (check-equal? (run '(add1 (sub1 7))) 7)

  ;; Con examples
  (check-equal? (run '(if (zero? 0) 1 2)) 1)
  (check-equal? (run '(if (zero? 1) 1 2)) 2)
  (check-equal? (run '(if (zero? -7) 1 2)) 2)
  (check-equal? (run '(if (zero? 0)
                          (if (zero? 1) 1 2)
                          7))
                2)
  (check-equal? (run '(if (zero? (if (zero? 0) 1 0))
                          (if (zero? 1) 1 2)
                          7))
                7)

  ;; Dupe examples
  (check-equal? (run #t) #t)
  (check-equal? (run #f) #f)
  (check-equal? (run (if #t 1 2)) 1)
  (check-equal? (run (if #f 1 2)) 2)
  (check-equal? (run (if 0 1 2)) 1)
  (check-equal? (run '(if #t 3 4)) 3)
  (check-equal? (run '(if #f 3 4)) 4)
  (check-equal? (run '(if  0 3 4)) 3)
  (check-equal? (run '(zero? 4)) #f)
  (check-equal? (run '(zero? 0)) #t)

  ;; Dodger examples
  (check-equal? (run #\a) #\a)
  (check-equal? (run #\b) #\b)
  (check-equal? (run '(char? #\a)) #t)
  (check-equal? (run '(char? #t)) #f)
  (check-equal? (run '(char? 8)) #f)
  (check-equal? (run '(char->integer #\a)) (char->integer #\a))
  (check-equal? (run '(integer->char 955)) #\λ))

(test-runner (λ (e) (interp (parse e))))
(test-runner (λ (e) (asm-interp (compile (parse e)))))

(define (test-runner-io run)
  ;; Evildoer examples
  (check-equal? (run 7 "") "7\n")
  (check-equal? (run '(write-byte 97) "") "a")
  (check-equal? (run '(read-byte) "a") "97\n")
  (check-equal? (run '(begin (write-byte 97) (read-byte)) "b") "a98\n")
  (check-equal? (run '(read-byte) "") "#<eof>\n")
  (check-equal? (run '(eof-object? (read-byte)) "") "#t\n")
  (check-equal? (run '(eof-object? (read-byte)) "a") "#f\n")
  (check-equal? (run '(begin (write-byte 97) (write-byte 98)) "") "ab"))
  

(test-runner-io (λ (e s) (interp/io (parse e) s)))
(test-runner-io (λ (e s) (asm-interp/io (compile (parse e)) s)))
