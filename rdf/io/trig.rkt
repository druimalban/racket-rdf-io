#lang racket/base

(require rdf/core/io
         rdf/core/nsmap
         ;; --------------------------------------
         "./base.rkt")

(provide trig-representation)

(define representation-name "TriG")

(define (trig-write-dataset dataset (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (write-trig-dataset dataset out))

(define (trig-write-graph graph (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (write-trig-graph graph out))

(define trig-representation
 (representation
   'trig
   representation-name
   '("trig")
   #f
   trig-write-dataset
   trig-write-graph))
