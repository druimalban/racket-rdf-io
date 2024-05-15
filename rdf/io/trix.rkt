#lang racket/base

(require racket/function
         racket/list
         racket/string
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         xml
         xml/ns
         ;; --------------------------------------
         rdf/core/dataset
         rdf/core/literal
         rdf/core/nsmap
         rdf/core/statement
         rdf/core/graph
         rdf/core/triple
         ;; --------------------------------------
         media-type
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

(define hpl-namespace "http://jena.sourceforge.net/TriX/")
(define hpl-root-element-name 'graphset)

(define w3c-namespace "http://www.w3.org/2004/03/trix/trix-1/")
(define w3c-root-element-name 'trix)
(define w3c-root-element-name-alt 'TriX)

(define xml-namespace-string "http://www.w3.org/XML/1998/namespace")

;; -------------------------------------------------------------------------------------------------
;; Internal Writer
;; -------------------------------------------------------------------------------------------------

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
  (let ((element-name (if hpl-variant hpl-root-element-name
                          w3c-root-element-name))
        (namespace (if hpl-variant
                       hpl-namespace
                       w3c-namespace)))
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
;; Internal Reader
;; -------------------------------------------------------------------------------------------------

(define (make-context elt (parent (make-hash '())))
  (let ((new-ctx (hash-copy parent)))
    (for-each
     (λ (att)
       (when (attribute-name/ns=? att 'lang xml-namespace-string)
         (hash-set! new-ctx 'lang (attribute-value att))))
     (element-attributes elt))
    new-ctx))

(define (element->statement-part ns elt ctx)
  (if (string=? (element-namespace elt) ns)
      (let ((type (element-name elt))
            (content (string-trim
                      (apply string-append
                             (map pcdata-string (filter pcdata? (element-content elt)))))))
        (cond
          ((eq? type 'uri)
           (string->url content))
          ((eq? type 'id)
           (make-blank-node content))
          ((eq? type 'plainLiteral)
           (if (hash-has-key? ctx 'lang)
               (make-lang-string-literal content (hash-ref ctx 'lang))
               (make-untyped-literal content)))
          ((eq? type 'typedLiteral)
           (let ((datatype (map string->url
                                (map attribute-value
                                     (filter
                                      (λ (att)
                                       (attribute-name/optional-ns=? att 'datatype ns))
                                      (element-attributes elt))))))
             (if (not (empty? datatype))
                 (make-typed-literal content (first datatype))
                 (raise-representation-read-error
                  'trix "[datatype]" "empty?"))))
          (else (raise-representation-read-error
                 'trix "<uri|id|plainLiteral|typedLiteral>" (element-name elt)))))
      (raise-representation-read-error
       'trix (format "namespace ~s" ns) (element-namespace elt))))

(define (element->statement ns elt ctx)
   (if (element-name/ns=? elt 'triple ns)
      (let ((children (filter element? (element-content elt))))
        (triple
         (element->statement-part ns (first children) (make-context (first children) ctx))
         (element->statement-part ns (second children) (make-context (second children) ctx))
         (element->statement-part ns (third children) (make-context (third children) ctx))))
      (raise-representation-read-error
       'trix "<triple>" (symbol->string (element-name elt)))))

(define (element->graph ns elt ctx)
  (if (element-name/ns=? elt 'graph ns)
      (let* ((asserted (map
                        (λ (v) (cond
                                 ((string=? v "true") #t)
                                 ((string=? v "false") #f)
                                 (else (raise-representation-read-error
                                        'xml "boolean?" v))))
                        (filter
                         (λ (att) (attribute-name/ns=? att 'asserted ns))
                         (element-attributes elt))))
             (asserted (if (empty? asserted) #t (first asserted)))
             (children (filter element? (element-content elt)))
             (maybe-name (if (empty? children) #f (first children)))
             (statement-fn (λ (elt) (element->statement ns elt (make-context elt ctx)))))
        (cond
          ((empty? children)
           (unnamed-graph '()))
          ((or (element-name/ns=? maybe-name 'uri ns)
                 (element-name/ns=? maybe-name 'id ns))
           (let ((content (string-trim
                           (apply string-append
                                  (map pcdata-string
                                       (filter pcdata? (element-content maybe-name)))))))
             (named-graph
              (if (eq? (element-name maybe-name) 'uri)
                  (string->url content)
                  (make-blank-node content))
              (map statement-fn (rest children))
              #:asserted asserted)))
          (else
           (unnamed-graph (map statement-fn children) #:asserted asserted))))
      (raise-representation-read-error
       'trix "<graph>" (symbol->string (element-name elt)))))

(define (document->dataset doc)
  (let* ((root (document-element doc))
         (ns (cond
               ((element-name/ns=? root hpl-root-element-name hpl-namespace)
                hpl-namespace)
               ((or (element-name/ns=? root w3c-root-element-name w3c-namespace)
                    (element-name/ns=? root w3c-root-element-name-alt w3c-namespace))
                w3c-namespace)
               (else (raise-representation-read-error
                      'xml
                      "<graphset|trix|TriX>"
                      (symbol->string (element-name root)))))))
    (graph-list->dataset (map
                      (λ (elt) (element->graph ns elt (make-context elt)))
                      (filter element? (element-content root))))))

;; -------------------------------------------------------------------------------------------------
;; Provided Reader, Writer, and Representation
;; -------------------------------------------------------------------------------------------------

(define (trix-read (inp (current-input-port)))
  (let* ((initial (read-xml inp))
         (document (xml-expand-names initial)))
    (document->dataset document)))

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
   (string->media-type "application/trix+xml")
   '("trix" "trxml")
   trix-read
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
   (string->media-type "application/trix+xml")
   '("trix" "trxml")
   trix-read
   trix-write-dataset/w3c
   trix-write-graph/w3c))
