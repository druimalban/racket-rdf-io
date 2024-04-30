#lang racket/base

(require net/url-string
         "../../core/graph.rkt"
         "../../core/literal.rkt"
         "../../core/statement.rkt"
         "../nquads.rkt")

(define int-42 (make-typed-string "42" (string->url "http://www.w3.org/2001/XMLSchema#integer")))

(define *test-graph*
  (make-default-graph
   (make-statement-list "http://example.com/p/me"
                        (list (list "http://example.com/v/people#hasFirstName" (list (make-language-string "Me" "en")))
                              (list "http://example.com/v/people#hasLastName" (list "!"))
                              (list "http://example.com/v/people#hasScores" (list 2 4 int-42))))))

(nquad-writer *test-graph*)
