#lang racket/base

(require rackunit
         rackunit/text-ui
         ;; --------------------------------------
         "./data.rkt")

(provide json-reader-test-suite
         json-writer-test-suite)

;; -----------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -----------------------------------------------------------------------------------------------

(define json-reader-test-suite
  (test-suite
   "Test JSON reader"

   (test-case
       "empty-graph"
     (check-reader "empty-graph" 'json *empty-graph*))

   (test-case
       "example-graph-1"
      (check-reader "example-graph-1" 'json *example-unnamed-graph-1*))
   ))

;; (run-tests json-reader-test-suite)
(define json-writer-test-suite
  (test-suite
   "Test JSON writer"

   (test-case
       "empty-graph"
     (check-write-graph *empty-graph* 'json "empty-graph"))

   (test-case
       "empty-named-graph"
     (check-write-graph *empty-named-graph* 'json "empty-named-graph"))

   (test-case
       "example-graph-1"
     (check-write-graph *example-graph-1* 'json "example-graph-1"))
   ))

;; -----------------------------------------------------------------------------------------------
;; Test Runner
;; -----------------------------------------------------------------------------------------------

(run-tests json-reader-test-suite)
(run-tests json-writer-test-suite)
