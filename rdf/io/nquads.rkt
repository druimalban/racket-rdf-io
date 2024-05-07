#lang racket/base

(require racket/port
         ;; --------------------------------------
         rdf/core/io
         rdf/core/nsmap
         ;; --------------------------------------
         "./base.rkt"
         ;; --------------------------------------
         "./private/line-oriented.rkt")

(provide nquad-representation)

(define (nq-read (inp (current-input-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (let ((lines (port->lines inp #:close? #t)))
    (map line->quad lines)))

(define (nq-write-dataset dataset (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (write-nquad-dataset dataset out))

(define (nq-write-graph graph (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (write-nquad-graph graph out))

(define nquad-representation
 (representation
   'nquads
   "N-Quads"
   '("nq")
   nq-read
   nq-write-dataset
   nq-write-graph))
