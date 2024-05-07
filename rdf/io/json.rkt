#lang racket/base

(require racket/function
         racket/set
         ;; --------------------------------------
         net/url-string
         net/url-structs
         ;; --------------------------------------
         json
         ;; --------------------------------------
         rdf/core/graph
         rdf/core/literal
         rdf/core/nsmap
         rdf/core/statement
         ;; --------------------------------------
         "./base.rkt")

;; In general, a triple (subject S, predicate P, object O) is serialized in the following structure:
;;
;; { "S" : { "P" : [ O ] } }
;;
;; The object of the triple O is represented as a further JSON object with the following keys:
;;
;; - type :: one of 'uri', 'literal' or 'bnode' (required)
;; - value :: the URI of the object, its lexical value or a blank node label depending on whether the object is a
;;   uri, literal or bnode
;; - lang :: the language of a literal value (optional but if supplied it must not be empty)
;; - datatype :: the datatype URI of the literal value (optional)
;;
;; Blank node subjects are named using a string conforming to the nodeID production in Turtle. For example: _:A1
;;
;; The 'lang' and 'datatype' keys should only be used if the value of the 'type' key is "literal".

(define (object->json object)
  (make-hash (cond
               ((url? object)
               (list (cons 'uri (url->string object))))
               ((blank-node? object)
                (list (cons 'bnode (format "_:~a" (blank-node->string object)))))
               ((literal? object)
                (displayln object)
                (let ((literal (list (cons 'value (literal-lexical-form object)))))
                  (cond
                    ((has-language-tag? object)
                     (cons (cons 'lang (literal-language-tag object)) literal))
                    ((has-datatype-iri? object)
                     (cons (cons 'datatype (url->string (literal-datatype-iri object))) literal))
                    (else literal)))))))

(define (predicate->json predicate objects)
  (cons (string->symbol (url->string predicate))
        (map object->json objects)))

(define (subject->json subject predicates)
  (cons (cond
          ((url? subject)
           (string->symbol (url->string subject)))
          ((blank-node? subject)
           (format "_:~a" (blank-node->string subject))))
        (make-hash (hash-map predicates predicate->json))))

(define (tree->json tree)
  (make-hash (hash-map tree subject->json)))

;; -------------------------------------------------------------------------------------------------

(define (json-write-graph graph (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (write-json (tree->json (graph->tree graph)) out)
  (newline out))

(define json-representation
  (representation
   'json
   "JSON"
   '("json")
   #f
   #f
   json-write-graph))


(require "./tests/data.rkt")
(json-write-graph *example-graph-1*)
