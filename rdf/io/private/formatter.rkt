#lang racket/base

(require racket/contract)

(provide (except-out (struct-out formatter)
                     mkformatter
                     set-formatter-level!)
         make-formatter
         (contract-out
          (indent (->* (formatter?) (exact-nonnegative-integer?) formatter?))
          (outdent (->* (formatter?) (exact-nonnegative-integer?) formatter?))
          (indentation-length (-> formatter? exact-nonnegative-integer?))
          (indentation-string (-> formatter? string?))))

(struct formatter (indent-char indent-by line-width (level #:mutable))
  #:transparent
  #:constructor-name mkformatter
  #:guard (struct-guard/c char? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?))

(define (make-formatter #:char (indent-char #\space) #:indent-by (indent-by 2) #:line-width (line-width 80))
  (mkformatter indent-char indent-by line-width 0))

(define (indent fmt (by 1))
  (set-formatter-level! fmt (+ (formatter-level fmt) by))
  fmt)

(define (outdent fmt (by 1))
  (set-formatter-level! fmt (- (formatter-level fmt) by))
  fmt)

(define (indentation-length fmt)
  (* (formatter-indent-by fmt) (formatter-level fmt)))

(define (indentation-string fmt)
  (make-string (indentation-length fmt)
               (formatter-indent-char fmt)))
