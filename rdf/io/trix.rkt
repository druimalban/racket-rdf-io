#lang racket/base

(require racket/function
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         rdf/core/dataset
         rdf/core/literal
         rdf/core/statement
         rdf/core/graph
         ;; --------------------------------------
         "./base.rkt"
         ;; --------------------------------------
         "./private/writexml.rkt")

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

(define (trix-write-statement-part val out)
  (let ((indent "      "))
    (cond
      ((url? val)
       (display (element "uri" #:content (url->string val)) out))

      ((blank-node? val)
       (display (element "id" #:content (blank-node->string val)) out))

      ((has-language-tag? val)
       (display (element
                 "plainLiteral"
                 #:attrs (list (xml:lang (literal-language-tag val)))
                 #:content (literal-lexical-form val))
                out))

      ((has-datatype-iri? val)
       (display (element
                 "typedLiteral"
                 #:attrs (list (cons "datatype" (url->string (literal-datatype-iri val))))
                 #:content (literal-lexical-form val))
                out))

      ((literal? val)
       (display (element "plainLiteral" #:content val) out))

     (else (error "Not a subject?")))))

(define (trix-write-statement/impl stmt out)
  (display (element-start "triple") out)
  (trix-write-statement-part (get-subject stmt) out)
  (trix-write-statement-part (get-predicate stmt) out)
  (trix-write-statement-part (get-object stmt) out)
  (display (element-end "triple") out))

(define (trix-write-graph/impl graph hpl-variant out)
  (if hpl-variant
      (display (element-start "graph"
                                (list (cons 'asserted
                                            (if (graph-asserted graph)
                                                "true" "false")))) out)
      (display (element-start "graph") out))
  (when (graph-named? graph)
    (display (element "uri" #:content (url->string (graph-name graph))) out))
  (for-each
   (curryr trix-write-statement/impl out)
   (graph-statements graph))
  (display (element-end "graph") out))

(define (trix-write-dataset/impl dataset hpl-variant out)
  (let ((element-name (if hpl-variant "graphset" "trix"))
        (namespace (if hpl-variant
                       "http://jena.sourceforge.net/TriX/"
                       "http://www.w3.org/2004/03/trix/trix-1/")))
    (display (standard-prolog-entry) out)
    (display (element-start element-name (list (xml:namespace namespace))) out)
    (for-each
     (λ (graph) (trix-write-graph/impl graph hpl-variant out))
     (dataset-values dataset))
    (display (element-end element-name) out)))

;; -------------------------------------------------------------------------------------------------

(define (trix-write-dataset dataset (out (current-output-port)))
  (trix-write-dataset/impl dataset #t out))

(define (trix-write-dataset/w3c dataset (out (current-output-port)))
  (trix-write-dataset/impl dataset #f out))

(define (trix-write-graph graph (out (current-output-port)))
  (trix-write-dataset (graph-list->dataset (list graph)) out))

(define (trix-write-graph/w3c graph (out (current-output-port)))
  (trix-write-dataset/w3c (graph-list->dataset (list graph)) out))

(define (trix-write-statement stmt (out (current-output-port)))
  (raise-representation-write-error
   representation-name
   'dataset
   '((unsupported . "cannot write individual statements"))))

(define (trix-write-literal lit (out (current-output-port)))
  (raise-representation-write-error
   representation-name
   'dataset
   '((unsupported . "cannot write individual literals"))))

(define trix-representation
  (representation
   'trix
   "TriX"
   '("trix" "trxml")
   #f
   (writer trix-write-dataset
           trix-write-graph
           trix-write-statement
           trix-write-literal)))

(define trix-representation/w3c
  (representation
   'trix-w3c
   "TriX (W3C)"
   '("trix" "trxml")
   #f
   (writer trix-write-dataset/w3c
           trix-write-graph/w3c
           trix-write-statement
           trix-write-literal)))
