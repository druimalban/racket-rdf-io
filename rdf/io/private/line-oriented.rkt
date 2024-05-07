#lang racket/base

(require racket/bool
         racket/list
         racket/port
         racket/string
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         rdf/core/graph
         rdf/core/io
         rdf/core/literal
         rdf/core/nsmap
         rdf/core/quad
         rdf/core/statement
         rdf/core/triple
         ;; --------------------------------------
         "../base.rkt")

(provide line->triple
         line->quad)

(define url-regexp "<[^>]+>")

(define blank-node-regexp "_:\\w+")

(define subject-regexp (format "(~a|~a)" url-regexp blank-node-regexp))

(define predicate-regexp (format "(~a)" url-regexp))

(define literal-regexp (format "(\"[^\"]*\")(@[a-zA-Z0-9-]+|\\^\\^~a)?" url-regexp))

(define object-regexp (format "(~a|~a|~a)" url-regexp blank-node-regexp literal-regexp))

(define statement-regexp (format "\\s*~a\\s+~a\\s+~a\\s+.\\s*"
                                 subject-regexp
                                 predicate-regexp
                                 object-regexp))

(define statement-actual (pregexp statement-regexp))

(define graph-name-regexp subject-regexp)

(define graph-statement-regexp (format "\\s*~a\\s+~a\\s+~a\\s+~a\\s+.\\s*"
                                 subject-regexp
                                 predicate-regexp
                                 object-regexp
                                 graph-name-regexp))

(define graph-statement-actual (pregexp graph-statement-regexp))

(define (match->subject repr s)
  (cond
    ((and (string-prefix? s "<") (string-suffix? s ">"))
     (string->url (substring s 1 (- (string-length s) 1))))
    ((string-prefix? s "_:")
     (make-blank-node (substring s 2)))
    (else (raise-representation-read-error repr "subject?" s))))

(define (match->predicate repr s)
  (string->url (substring s 1 (- (string-length s) 1))))

(define (match->object repr s sopt)
  (cond
    ((and (string-prefix? s "<") (string-suffix? s ">"))
     (string->url (substring s 1 (- (string-length s) 1))))
    ((string-prefix? s "_:")
     (make-blank-node (substring s 2)))
    ((and (string-prefix? s "\"")(string-suffix? s "\"") (false? sopt))
     (make-untyped-literal (substring s 1 (- (string-length s) 1))))
    ((and (string-prefix? s "\"")(string-suffix? s "\"") (string-prefix? sopt "@"))
     (make-lang-string-literal
      (substring s 1 (- (string-length s) 1))
      (substring sopt 1)))
    ((and (string-prefix? s "\"")(string-suffix? s "\"")
          (string-prefix? sopt "^^<") (string-suffix? sopt ">"))
     (make-typed-literal
      (substring s 1 (- (string-length s) 1))
      (string->url (substring sopt 3 (- (string-length sopt) 1)))))
    (else (raise-representation-read-error repr "object?" s `((additional . ,sopt))))))

(define match->graph-name match->subject)

(define (line->triple line)
  (let ((matches (regexp-match statement-actual line)))
    (if (false? matches)
        (raise-representation-read-error 'ntriple "triple?" line)
        (triple
           (match->subject 'ntriple (second matches))
           (match->predicate 'ntriple (third matches))
           (match->object 'ntriple (if (fifth matches) (fifth matches) (fourth matches)) (sixth matches))))))

(define (line->quad line)
  (let ((matches (regexp-match statement-actual line)))
    (if (false? matches)
        (raise-representation-read-error 'nquads "quad?" line)
        (quad
           (match->subject'nquads (second matches))
           (match->predicate'nquads (third matches))
           (match->object'nquads (fifth matches) (sixth matches))
           (match->graph-name 'nquads (seventh matches))))))
