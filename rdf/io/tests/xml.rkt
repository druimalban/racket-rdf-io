#lang racket/base

(require rackunit
         rackunit/text-ui
         ;; --------------------------------------
         "./data.rkt")

(provide xml-test-suite)

;; -----------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -----------------------------------------------------------------------------------------------

(define xml-test-suite
  (test-suite
   "Test xml"

   (test-case
       "empty-graph"
     (check-write-graph *empty-graph* 'xml "empty-graph"))

   (test-case
       "empty-named-graph"
     (check-write-graph *empty-named-graph* 'xml "empty-named-graph"))

   (test-case
       "example-graph-1"
     (check-write-graph *example-graph-1* 'xml "example-graph-1"))
   ))

;; -----------------------------------------------------------------------------------------------
;; Test Runner
;; -----------------------------------------------------------------------------------------------

(run-tests xml-test-suite)
