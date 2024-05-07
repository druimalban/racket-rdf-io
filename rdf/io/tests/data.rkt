#lang racket/base

(require racket/file
         racket/port
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         rackunit
         ;; --------------------------------------
         rdf/core/dataset
         rdf/core/graph
         rdf/core/literal
         rdf/core/namespace
         rdf/core/triple
         rdf/core/v/rdf
         ;; --------------------------------------
         "../base.rkt"
         "../registry.rkt")

(provide (all-defined-out))

(define *example-language-literal* (make-lang-string-literal "Me" "en"))

(define *example-typed-literal*
  (make-typed-literal
   "42"
   (string->url "http://www.w3.org/2001/XMLSchema#integer")))

(define *empty-dataset* (unnamed-dataset (make-hash)))

(define *empty-graph* (unnamed-graph '()))

(define *empty-named-graph* (named-graph (string->url "http://example.org/example/named") '()))

(define *example-graph-1*
  (named-graph
   (string->url "http://example.com/peeps")
   (statement-list
    "http://example.com/p/me"
    (list (list "http://example.com/v/people#hasFirstName"
                (list *example-language-literal*))
          (list "http://example.com/v/people#hasLastName"
                (list "!"))
          (list "http://example.com/v/people#hasScores"
                (list 2 4 *example-typed-literal*))))))

(define *example-type-statement*
  (triple
   (string->url "http://example.com/peeps")
   (nsname->url rdf:type)
   (string->url "http://xmlns.com/foaf/0.1/Person")))

(define (check-reader test-data repr-id expected)
  (let* ((representation (get-representation repr-id))
         (reader (representation-reader representation))
         (actual (with-input-from-string
                   test-data
                   (λ () (reader)))))
    (check-equal? actual expected)))


(define (check-writer test-data repr-id expected-results-file accessor)
  (let* ((representation (get-representation repr-id))
         (writer (accessor representation))
         (actual (with-output-to-string
                   (λ () (writer test-data))))
         (expected (file->string
                    (format "~a.~a"
                            expected-results-file
                            repr-id)
                    #:mode 'text)))
    (check-equal? actual expected)))

(define (check-write-dataset test-data repr-id expected-results-file)
  (check-writer test-data repr-id expected-results-file representation-dataset-writer))

(define (check-write-graph test-data repr-id expected-results-file)
  (check-writer test-data repr-id expected-results-file representation-graph-writer))
