#lang racket/base

(require racket/port
         ;; --------------------------------------
         rdf/core/io
         rdf/core/nsmap
         ;; --------------------------------------
         media-type
         ;; --------------------------------------
         "./base.rkt"
         ;; --------------------------------------
         "./private/line-oriented.rkt")

(provide ntriple-representation)

(define (nt-read (inp (current-input-port)))
  (let ((lines (port->lines inp #:close? #t)))
    (map line->triple lines)))

(define (nt-write-graph graph (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (write-ntriple-graph graph out))

(define ntriple-representation
 (representation
   'ntriples
   "N-Triples"
   (string->media-type "application/n-triples")
   '("nt")
   nt-read
   #f
   nt-write-graph))
