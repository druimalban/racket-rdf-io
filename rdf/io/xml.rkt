#lang racket/base

(require racket/bool
         racket/function
         racket/set
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         rdf/core/namespace
         rdf/core/dataset
         rdf/core/literal
         rdf/core/nsmap
         rdf/core/statement
         rdf/core/graph
         rdf/core/v/rdf
         ;; --------------------------------------
         "./base.rkt"
         ;; --------------------------------------
         "./private/formatter.rkt"
         "./private/writexml.rkt")

(provide xml-representation)

(define representation-name "RDF/XML")

(define (type-attributes object)
  (cond
    ((url? object)
     (list (cons "rdf:resource" (url->string object))))
    ((blank-node? object)
     (list (cons "rdf:nodeID" (blank-node->string object))))
    ((and (literal? object) (has-language-tag? object))
     (list (xml:lang (literal-language-tag object))))
    ((and (literal? object) (has-datatype-iri? object))
     (list (cons "rdf:datatype" (url->string (literal-datatype-iri object)))))
    (else '())))

(define (xml-property predicate object nsmap out)
  (let* ((ns+name (url->namespace+name predicate))
         (ns-url (car ns+name))
         (ns-prefix (nsmap-prefix-ref nsmap (url->namespace ns-url)))
         (ns-attr (if (false? ns-prefix)
                      (list (xml:namespace (url->string ns-url)))
                      (list)))
         (name (if (false? ns-prefix)
                   (cdr ns+name)
                   (format "~a~a" (prefix->string ns-prefix) (cdr ns+name)))))
    (display (element-start name #:attrs (append ns-attr
                                                 (type-attributes object)))
             out)
    (when (literal? object)
      (display (element-content (literal-lexical-form object)) out))
    (display (element-end name) out)))

(define (xml-describe graph subject statement-list nsmap out)
  (let ((subject (if (blank-node? subject)
                     (cons "rdf:nodeID" (blank-node->string subject))
                     (cons "rdf:about" (url->string subject)))))
    (display (element-start "rdf:Description" #:attrs (list subject)) out)
    (for-each (λ (stmt) (xml-property (get-predicate stmt) (get-object stmt) nsmap out))
              statement-list)
    (display (element-end "rdf:Description")) out))

(define (xml-write-graph/impl graph nsmap out)
  (for-each (λ (subject)
              (xml-describe graph
                            subject
                            (graph-filter-by-subject graph subject)
                            nsmap
                            out))
            (set->list (graph-distinct-subjects graph))))

(define (nsmap->xmlns nsmap)
  (let ((initial-list (map (λ (pair) (xml:namespace (namespace->string (cdr pair))
                                                    (prefix->name-string (car pair))))
                           (nsmap->list nsmap)))
        (rdf-prefix (nsmap-prefix-ref nsmap rdf:)))
    (if (false? rdf-prefix)
        (cons (xml:namespace rdf-namespace-string
                             rdf-prefix-string)
            initial-list)
      initial-list)))

(define (xml-write-dataset/impl dataset nsmap out)
    (display (standard-prolog-entry) out)
  (display (element-start "rdf:RDF" #:attrs (nsmap->xmlns (or nsmap (make-common-nsmap)))) out)
    (for-each
     (λ (graph) (xml-write-graph/impl graph nsmap out))
     (dataset-values dataset))
  (display (element-end "rdf:RDF") out))

;; -------------------------------------------------------------------------------------------------

(define (xml-write-dataset dataset (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (xml-write-dataset/impl dataset nsmap out))

(define (xml-write-graph graph (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (xml-write-dataset (graph-list->dataset (list graph)) out #:map nsmap))

(define (xml-write-statement stmt (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (raise-representation-write-error
   representation-name
   'dataset
   '((unsupported . "cannot write individual statements"))))

(define (xml-write-literal lit (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (raise-representation-write-error
   representation-name
   'dataset
   '((unsupported . "cannot write individual literals"))))

(define xml-representation
  (representation
   'xml
   representation-name
   '("xml" "rdf")
   #f
   (writer xml-write-dataset
           xml-write-graph
           xml-write-statement
           xml-write-literal)))

(require "./tests/data.rkt")

(let ((nsmap (make-common-nsmap)))
  (nsmap-set! nsmap (string->prefix "peeps") (string->namespace "http://example.com/v/people#"))
  (parameterize ((xml-formatter (make-formatter)))
    (xml-write-graph *test-graph* #:map nsmap)))
