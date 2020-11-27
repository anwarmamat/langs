#lang racket
(provide asm-string)
(require "ast.rkt")

;; Instruction -> String
(define (instr->string i)
  (match i
    [(Ret)       "\tret\n"]
    [(Label l)   (string-append (label-symbol->string l) ":\n")]
    [(Mov a1 a2)
     (string-append "\tmov "
                    (arg->string a1) ", "
                    (arg->string a2) "\n")]
    [(Add a1 a2)
     (string-append "\tadd "
                    (arg->string a1) ", "
                    (arg->string a2) "\n")]
    [(Sub a1 a2)
     (string-append "\tsub "
                    (arg->string a1) ", "
                    (arg->string a2) "\n")]    
    [(Cmp a1 a2)
     (string-append "\tcmp "
                    (arg->string a1) ", "
                    (arg->string a2) "\n")]
    [(And a1 a2)
     (string-append "\tand "
                    (arg->string a1) ", "
                    (arg->string a2) "\n")]
    [(Or a1 a2)
     (string-append "\tor "
                    (arg->string a1) ", "
                    (arg->string a2) "\n")]
    [(Xor a1 a2)
     (string-append "\txor "
                    (arg->string a1) ", "
                    (arg->string a2) "\n")]    
    [(Jmp l)
     (string-append "\tjmp "
                    (label-symbol->string l) "\n")]
    [(Je l)
     (string-append "\tje "
                    (label-symbol->string l) "\n")]
    [(Jne l)
     (string-append "\tjne "
                    (label-symbol->string l) "\n")]
    [(Call l)
     (string-append "\tcall "
                    (label-symbol->string l) "\n")]
    [(Push r)
     (string-append "\tpush "
                    (reg->string r) "\n")]
    [(Pop r)
     (string-append "\tpop "
                    (reg->string r) "\n")]))

;; Arg -> String
(define (arg->string a)
  (match a
    [(? reg?) (reg->string a)]
    [(Offset (? reg? r) i)
     (string-append "[" (reg->string r) " + " (number->string (* i 8)) "]")]
    [(? integer?) (number->string a)]))

;; Any -> Boolean
(define (reg? x)
  (and (symbol? x)
       (memq x '(rax rbx rdi rsp))))

;; Reg -> String
(define (reg->string r)
  (symbol->string r))

;; Label -> String
;; prefix with _ for Mac
(define label-symbol->string
  (match (system-type 'os)
    ['macosx
     (λ (s) (string-append "_" (symbol->string s)))]
    [_ symbol->string]))

;; Asm -> String
(define (asm-string a)
  ;; entry point will be first label
  (match (findf Label? a)
    [(Label g)
     (string-append
      "\tglobal " (label-symbol->string g) "\n"
      "\textern " (label-symbol->string 'error) "\n"
      "\tsection .text\n"      
      (foldr (λ (i s) (string-append (instr->string i) s)) "" a))]))

