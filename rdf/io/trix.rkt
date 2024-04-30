#lang racket/base

(require racket/string
         net/url-string
         "../core/graph.rkt"
         "../core/literal.rkt"
         "../core/statement.rkt"
         "./lib.rkt")

(provide *trix-serialization*
         trix-writer)

(define (trix-write-between val indent element out (attributes '()))
  (let ((attribute-string (string-join
                           (map (λ (pair) (format " ~a=~s" (car pair) (cadr pair)))
                                attributes)
                           "")))
    (displayln (format "~a<~a~a>~a</~a>" indent element attribute-string val element) out)))

(define (trix-writer-part val out)
  (let ((indent "      "))
   (cond
     ((url? val)
      (trix-write-between (url->string val) indent "uri" out))

     ((blank-node? val)
      (trix-write-between (blank-node-id val) indent "id" out))

     ((language-string? val)
      (trix-write-between (language-string-text val) indent "plainLiteral" out
                          (list (list "xml:lang" (language-string-language val)))))

     ((typed-string? val)
      (trix-write-between (typed-string-text val) indent "typedLiteral" out
                          (list (list "datatype" (url->string (typed-string-datatype val))))))

     ((literal? val) (trix-write-between val indent "plainLiteral" out))

     (else (error "Not a subject?")))))

(define (trix-writer graph (out (current-output-port)))
  (displayln "<?xml version\"1.0\"?" out)
  (displayln "<TriX xmlns=\"http://www.w3.org/2004/03/trix/trix-1/\">" out)
  (displayln "  <graph>" out)
  (when (graph-named? graph)
    (trix-write-between (url->string (graph-name graph)) "    " "uri" out))
  (for-each
   (λ (stmt)
     (displayln "    <triple>" out)
     (trix-writer-part (statement-subject stmt) out)
     (trix-writer-part (statement-predicate stmt) out)
     (trix-writer-part (statement-object stmt) out)
     (displayln "    </triple>" out))
   (graph-statements graph))
  (displayln "  </graph>" out)
  (displayln "</TriX>" out))

(define *trix-serialization*
  (make-serialization
   'trix
   "TriX"
   '("nt")
   (string->url "http://www.hpl.hp.com/techreports/2004/HPL-2004-56.html")
   trix-writer
   (void)))

(register-serialization *trix-serialization*)
