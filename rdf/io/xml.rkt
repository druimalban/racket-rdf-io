#lang racket/base

(require racket/bool
         racket/function
         racket/set
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         xml
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
         "./private/xmlu.rkt")

(provide xml-representation)

(define representation-name "RDF/XML")

(define (prefix+namespace->attribute prefix+ns)
  (let ((prefix (car prefix+ns))
        (namespace (cdr prefix+ns)))
    (make-xmlns-attribute
     (namespace->string namespace)
     #:prefix (if prefix (prefix->name-string prefix) prefix))))

(define (nsmap->xmlns nsmap)
  (let ((initial-list (map prefix+namespace->attribute
                           (nsmap->list nsmap)))
        (rdf-prefix (nsmap-prefix-ref nsmap rdf:)))
    (if (false? rdf-prefix)
        (cons (make-xmlns-attribute
               rdf-namespace-string
               #:prefix (substring rdf-prefix-string
                                   0
                                   (- (string-length rdf-prefix-string) 2)))
              initial-list)
        initial-list)))

(define (type-attributes object)
  (cond
    ((url? object)
     (list (make-attribute #f #f
                           'rdf:resource
                           (url->string object))))
    ((blank-node? object)
     (list (make-attribute #f #f
                           'rdf:nodeID
                           (blank-node->string object))))
    ((and (literal? object) (has-language-tag? object))
     (list (make-lang-attribute (literal-language-tag object))))
    ((and (literal? object) (has-datatype-iri? object))
     (list (make-attribute #f #f
                           'rdf:datatype
                           (url->string (literal-datatype-iri object)))))
    (else '())))

(define (xml-property predicate object nsmap)
  (let* ((ns+name (url->namespace+name predicate))
         (ns-url (car ns+name))
         (ns-prefix (nsmap-prefix-ref nsmap (url->namespace ns-url)))
         (ns-attr (if (false? ns-prefix)
                      (list (make-xmlns-attribute (url->string ns-url)))
                      (list)))
         (name (if (false? ns-prefix)
                   (cdr ns+name)
                   (format "~a~a" (prefix->string ns-prefix) (cdr ns+name)))))
    (make-element
     #f #f
     (string->symbol name)
     (append ns-attr (type-attributes object))
     (if (literal? object)
         (list (make-pcdata #f #f (literal-lexical-form object)))
         '()))))

(define (xml-describe subject statement-list nsmap)
  (let ((subject (if (blank-node? subject)
                     (make-attribute #f #f
                                     'rdf:nodeID (blank-node->string subject))
                     (make-attribute #f #f
                                     'rdf:about (url->string subject)))))
    (make-element
     #f #f
     'rdf:Description
     (list subject)
     (map (λ (stmt) (xml-property (get-predicate stmt) (get-object stmt) nsmap))
          statement-list))))

(define (graph->document graph nsmap)
  (make-document
   (make-prolog (list (make-xml-declaration "1.1" #:encoding 'UTF-8))
                #f
                '())
   (make-element
    #f #f
    'rdf:RDF
    (nsmap->xmlns (or nsmap (make-common-nsmap)))
    (map (λ (subject)
           (xml-describe subject
                         (graph-filter-by-subject graph subject)
                         nsmap))
         (set->list (graph-distinct-subjects graph))))
   '()))

;; -------------------------------------------------------------------------------------------------

(define (xml-write-graph graph (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (display-xml (graph->document graph nsmap) out
               #:indentation 'classic)
  (newline out))

(define xml-representation
  (representation
   'xml
   representation-name
   '("xml" "rdf")
   #f
   #f
   xml-write-graph))
