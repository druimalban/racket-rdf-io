#lang racket/base

(require net/url-string
         ;; --------------------------------------
         rdf/core/literal
         rdf/core/graph
         rdf/core/nsmap
         rdf/core/statement
         ;; --------------------------------------
         media-type
         text-table
         ;; --------------------------------------
         "./base.rkt")

(provide tabular-representation
         tabular-display-graph)

(define (subject->string component)
  (cond
    ((url? component)
     (url->string component))
    ((blank-node? component)
     (blank-node->string component))))

(define (predicate->string component)
  (cond
    ((url? component)
     (url->string component))))

(define (object->strings component)
  (cond
    ((url? component)
     (list (url->string component) "" ""))
    ((blank-node? component)
     (list (blank-node->string component) ""))
    ((literal? component)
     (list (literal-lexical-form component)
           (if (has-language-tag? component)
               (literal-language-tag component)
               "")
           (if (and (not (has-language-tag? component))
                    (has-datatype-iri? component))
               (url->string (literal-datatype-iri component))
               "")))))

(define (graph->table graph)
  (map (λ (stmt)
         (append (list (subject->string (get-subject stmt))
                       (predicate->string (get-predicate stmt)))
                 (object->strings (get-object stmt))))
       (graph-statements graph)))

(define table-columns '("Subject" "Predicate" "Object" "Language" "Datatype"))

(define (tabular-write-graph graph (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (let ((table-data (graph->table graph)))
    (displayln (table->string #:row-sep? '(#t #f ...)
                            #:col-sep? '(#t ...)
                            #:align '(left left right right left)
                            (cons table-columns table-data))
             out)))

(define (tabular-display-graph graph (out (current-output-port)))
  (tabular-write-graph graph out))

(define tabular-representation
 (representation
   'tabular
   "Text Tables"
   (string->media-type "text/plain")
   '("txt")
   #f
   #f
   tabular-write-graph))
