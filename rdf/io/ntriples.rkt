#lang racket/base

(require net/url-string
         "../core/graph.rkt"
         "../core/io.rkt"
         "./lib.rkt")

(provide *ntriple-serialization*
         ntriple-writer)

(define (ntriple-writer graph (out (current-output-port)))
  (write-ntriple-graph graph out))

(define *ntriple-serialization*
  (make-serialization
   'ntriples
   "N-Triples"
   '("nt")
   (string->url "https://www.w3.org/TR/n-triples/")
   ntriple-writer
   (void)))

(register-serialization *ntriple-serialization*)
