#lang racket/base

(require net/url-string
         ;; --------------------------------------
         rdf/core/graph
         rdf/core/io
         ;; --------------------------------------
         "./base.rkt")

(provide nquad-representation)

(define (nq-write-dataset dataset (out (current-output-port)))
  (write-nquad-dataset dataset out))

(define (nq-write-graph graph (out (current-output-port)))
  (write-nquad-graph graph out))

(define (nq-write-statement stmt (out (current-output-port)))
  (write-ntriple-statement stmt out))

(define (nq-write-literal lit (out (current-output-port)))
  (write-ntriple-literal lit out))

(define nquad-representation
 (representation
   'nquads
   "N-Quads"
   '("nq")
   #f
   (writer nq-write-dataset
           nq-write-graph
           nq-write-statement
           nq-write-literal)))
