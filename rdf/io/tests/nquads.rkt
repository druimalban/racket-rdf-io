#lang racket/base

(require "../base.rkt"
         "../registry.rkt"
         ;; --------------------------------------
         "./data.rkt")

(let* ((representation (get-representation 'nquads))
       (writer (representation-writer representation))
       (graph-writer (writer-graph writer)))
  (graph-writer *test-graph*))
