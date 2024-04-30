#lang racket/base

(require net/url-string
         ;; --------------------------------------
         rdf/core/graph
         rdf/core/literal
         rdf/core/triple)

(provide (all-defined-out))

(define int-42
  (make-typed-literal
   "42"
   (string->url "http://www.w3.org/2001/XMLSchema#integer")))

(define *test-graph*
  (named-graph
   (string->url "http://example.com/peeps")
   (statement-list
    "http://example.com/p/me"
    (list (list "http://example.com/v/people#hasFirstName"
                (list (make-lang-string-literal "Me" "en")))
          (list "http://example.com/v/people#hasLastName"
                (list "!"))
          (list "http://example.com/v/people#hasScores"
                (list 2 4 int-42))))))
