#lang racket/base

(require rackunit
         rackunit/text-ui
         ;; --------------------------------------
         rdf/core/dataset
         ;; --------------------------------------
         "./data.rkt")

(provide trix-reader-test-suite
         trix-writer-test-suite
         trix-w3c-writer-test-suite)

;; -----------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -----------------------------------------------------------------------------------------------

(define trix-reader-test-suite
  (test-suite
   "TriX reader"

  (test-case
      "empty-dataset"
    (check-reader "empty-dataset" 'trix *empty-dataset*))

  (test-case
      "empty-dataset (W3C)"
    (check-reader "empty-dataset" 'trix-w3c *empty-dataset*))

  (test-case
      "empty-graph"
    (check-reader "empty-graph" 'trix (graph-list->dataset (list *empty-graph*))))

  (test-case
      "empty-graph (W3C)"
    (check-reader "empty-graph" 'trix-w3c (graph-list->dataset (list *empty-graph*))))

  (test-case
      "empty-named-graph"
    (check-reader "empty-named-graph" 'trix (graph-list->dataset (list *empty-named-graph*))))

  (test-case
      "empty-named-graph (W3C)"
    (check-reader "empty-named-graph" 'trix-w3c (graph-list->dataset (list *empty-named-graph*))))

  (test-case
      "example-graph-1"
    (check-reader "example-graph-1" 'trix (graph-list->dataset (list *example-graph-1*))))

  (test-case
      "example-graph-1 (W3C)"
    (check-reader "example-graph-1" 'trix-w3c (graph-list->dataset (list *example-graph-1*))))

   ))

;; -----------------------------------------------------------------------------------------------

(define trix-writer-test-suite
  (test-suite
   "TriX writer"

   (test-case
       "empty-graph"
     (check-write-graph *empty-graph* 'trix "empty-graph"))

   (test-case
       "empty-named-graph"
     (check-write-graph *empty-named-graph* 'trix "empty-named-graph"))

   (test-case
       "example-graph-1"
     (check-write-graph *example-graph-1* 'trix "example-graph-1"))
))

;; -----------------------------------------------------------------------------------------------

(define trix-w3c-writer-test-suite
  (test-suite
   "TriX (W3C) writer"

   (test-case
       "empty-graph"
     (check-write-graph *empty-graph* 'trix-w3c "empty-graph"))

   (test-case
       "empty-named-graph"
     (check-write-graph *empty-named-graph* 'trix-w3c "empty-named-graph"))

   (test-case
       "example-graph-1"
     (check-write-graph *example-graph-1* 'trix-w3c "example-graph-1"))
))

;; -----------------------------------------------------------------------------------------------
;; Test Runner
;; -----------------------------------------------------------------------------------------------

(run-tests trix-reader-test-suite)
(run-tests trix-w3c-writer-test-suite)
(run-tests trix-writer-test-suite)
