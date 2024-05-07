#lang racket/base

(require racket/function
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         xml
         ;; --------------------------------------
         rdf/core/dataset
         rdf/core/literal
         rdf/core/nsmap
         rdf/core/statement
         rdf/core/graph
         ;; --------------------------------------
         "./base.rkt"
         ;; --------------------------------------
         "./private/xmlu.rkt")

(provide trix-representation
         trix-representation/w3c)

;; The W3C DTD:
;; <!-- TriX: RDF Triples in XML -->
;; <!ELEMENT trix         (graph*)>
;; <!ATTLIST trix         xmlns CDATA #FIXED "http://www.w3.org/2004/03/trix/trix-1/">
;; <!ELEMENT graph        (uri, triple*)>
;; <!ELEMENT triple       ((id|uri|plainLiteral|typedLiteral), uri, (id|uri|plainLiteral|typedLiteral))>
;; <!ELEMENT id           (#PCDATA)>
;; <!ELEMENT uri          (#PCDATA)>
;; <!ELEMENT plainLiteral (#PCDATA)>
;; <!ATTLIST plainLiteral xml:lang CDATA #IMPLIED>
;; <!ELEMENT typedLiteral (#PCDATA)>
;; <!ATTLIST typedLiteral datatype CDATA #REQUIRED>

;; the HPL-2003-268 DTD:
;; <!-- TriX: RDF Triples in XML -->
;; <!ELEMENT graphset (graph*)>
;; <!ATTLIST graphset xmlns CDATA #FIXED "http://example.org/TriX/">
;; <!ELEMENT graph ((id|uri)?, triple*)>
;; <!ATTLIST graph asserted (true|false) "true">
;; <!ELEMENT triple ((id|uri), uri, (id|uri|plainLiteral|typedLiteral))>
;; <!ELEMENT id (#PCDATA)>
;; <!ELEMENT uri (#PCDATA)>
;; <!ELEMENT plainLiteral (#PCDATA)>
;; <!ATTLIST plainLiteral xml:lang CDATA #IMPLIED>
;; <!ELEMENT typedLiteral (#PCDATA)>
;; <!ATTLIST typedLiteral datatype CDATA #REQUIRED>

;; In other examples the namespace is:
;;   "http://jena.sourceforge.net/TriX/"

(define (statement-part->element val nsmap)
  (let ((indent "      "))
    (cond
      ((url? val)
       (make-element #f #f
                     'uri
                     '()
                     (list (make-pcdata
                            #f #f
                            (url->string val)))))
      ((blank-node? val)
       (make-element #f #f
                     'id
                     '()
                     (list (make-pcdata
                            #f #f
                            (blank-node->string val)))))
      ((has-language-tag? val)
        (make-element #f #f
                     'plainLiteral
                     (list (make-attribute
                            #f #f
                            'xml:lang
                            (literal-language-tag val)))
                     (list (make-pcdata
                            #f #f
                            (literal-lexical-form val)))))
      ((has-datatype-iri? val)
        (make-element #f #f
                     'typedLiteral
                     (list (make-attribute
                            #f #f
                            'datatype
                            (url->string (literal-datatype-iri val))))
                     (list (make-pcdata
                            #f #f
                            (literal-lexical-form val)))))
      ((literal? val)
        (make-element #f #f
                     'plainLiteral
                     (list)
                     (list (make-pcdata
                            #f #f
                            (literal-lexical-form val)))))
     (else (error "Not a subject?")))))

(define (statement->element stmt nsmap)
  (make-element
   #f #f
   'triple
   '()
   (list (statement-part->element (get-subject stmt) nsmap)
         (statement-part->element (get-predicate stmt) nsmap)
         (statement-part->element (get-object stmt) nsmap))))

(define (graph->element graph hpl-variant nsmap)
  (let ((attributes (if hpl-variant
                        (list (make-attribute
                               #f #f
                               'asserted
                               (boolean->xml (graph-asserted graph))))
                        (list)))
        (elements (if (graph-named? graph)
                      (list (make-element
                             #f #f
                             'uri
                             (list)
                             (list (make-pcdata
                                    #f #f
                                    (url->string (graph-name graph))))))
                      (list))))
    (make-element
     #f #f
     'graph
     attributes
     (append elements
           (map
            (λ (stmt) (statement->element stmt nsmap))
            (graph-statements graph))))))

(define (dataset->document dataset hpl-variant nsmap)
  (let ((element-name (if hpl-variant 'graphset 'trix))
        (namespace (if hpl-variant
                       "http://jena.sourceforge.net/TriX/"
                       "http://www.w3.org/2004/03/trix/trix-1/")))
    (make-document
     (make-prolog (list (make-xml-declaration "1.1" #:encoding 'UTF-8))
                  #f
                  '())
     (make-element
      #f #f
      element-name
      (list (make-xmlns-attribute namespace))
      (map
       (λ (graph) (graph->element graph hpl-variant nsmap))
       (dataset-values dataset)))
     (list))))

;; -------------------------------------------------------------------------------------------------

(define (trix-write-dataset dataset (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (display-xml (dataset->document dataset #t nsmap) out)
  (newline out))

(define (trix-write-graph graph (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (trix-write-dataset (graph-list->dataset (list graph)) out #:map nsmap))

(define trix-representation
  (representation
   'trix
   "TriX"
   '("trix" "trxml")
   #f
   trix-write-dataset
   trix-write-graph))

;; -------------------------------------------------------------------------------------------------

(define (trix-write-dataset/w3c dataset (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (display-xml (dataset->document dataset #f nsmap) out)
  (newline out))

(define (trix-write-graph/w3c graph (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (trix-write-dataset/w3c (graph-list->dataset (list graph)) out #:map nsmap))

(define trix-representation/w3c
  (representation
   'trix-w3c
   "TriX (W3C)"
   '("trix" "trxml")
   #f
   trix-write-dataset/w3c
   trix-write-graph/w3c))
