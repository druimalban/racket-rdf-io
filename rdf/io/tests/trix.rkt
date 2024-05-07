#lang racket/base

(require rackunit
         rackunit/text-ui
        ;; --------------------------------------
         "./data.rkt")

(provide trix-test-suite
         trix-w3c-test-suite)

;; -----------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -----------------------------------------------------------------------------------------------

(define trix-test-suite
  (test-suite
   "Test trix"

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

(define trix-w3c-test-suite
  (test-suite
   "Test trix-w3c"

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

(run-tests trix-w3c-test-suite)
(run-tests trix-test-suite)
