#lang racket/base

(require racket/port
         ;; --------------------------------------
         rdf/core/io
         rdf/core/nsmap
         ;; --------------------------------------
         "./base.rkt"
         ;; --------------------------------------
         "./private/line-oriented.rkt")

(provide ntriple-representation)

(define (nt-read (inp (current-input-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (let ((lines (port->lines inp #:close? #t)))
    (map line->triple lines)))

(define (nt-write-graph graph (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (write-ntriple-graph graph out))

(define ntriple-representation
 (representation
   'ntriples
   "N-Triples"
   '("nt")
   nt-read
   #f
   nt-write-graph))
