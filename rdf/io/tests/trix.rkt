#lang racket/base

(require net/url-string
         ;; --------------------------------------
         rdf/core/graph
         rdf/core/literal
         rdf/core/statement
         rdf/core/triple
         ;; --------------------------------------
         "../base.rkt"
         "../registry.rkt"
        ;; --------------------------------------
         "./data.rkt")

(let* ((representation (get-representation 'trix))
       (writer (representation-writer representation))
       (graph-writer (writer-graph writer)))
  (graph-writer *test-graph*))
