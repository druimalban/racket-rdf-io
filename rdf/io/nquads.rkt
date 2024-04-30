#lang racket/base

(require net/url-string
         "../core/graph.rkt"
         "../core/io.rkt"
         "./lib.rkt")

(provide *nquad-serialization*
         nquad-writer)

(define (nquad-writer graph (out (current-output-port)))
  (write-nquad-graph graph out))

(define *nquad-serialization*
  (make-serialization
   'nquads
   "N-Quads"
   '("nt")
   (string->url "https://www.w3.org/TR/n-quads/")
   nquad-writer
   (void)))

(register-serialization *nquad-serialization*)
