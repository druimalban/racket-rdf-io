#lang racket/base

(require rdf/core/io
         ;; --------------------------------------
         "./base.rkt")

(provide ntriple-representation)

(define representation-name "N-Triples")

(define (nt-write-dataset dataset (out (current-output-port)))
  (raise-representation-write-error
   representation-name
   'dataset
   '((unsupported . "cannot write dataset"))))

(define (nt-write-graph graph (out (current-output-port)))
  (write-ntriple-graph graph out))

(define (nt-write-statement stmt (out (current-output-port)))
  (write-ntriple-statement stmt out))

(define (nt-write-literal lit (out (current-output-port)))
  (write-ntriple-literal lit out))

(define ntriple-representation
 (representation
   'ntriples
   representation-name
   '("nt")
   #f
   (writer nt-write-dataset
           nt-write-graph
           nt-write-statement
           nt-write-literal)))
