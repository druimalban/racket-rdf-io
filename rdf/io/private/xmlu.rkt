#lang racket/base

(require racket/bool
         racket/contract
         ;; --------------------------------------
         xml
         xml/ns
         ;; --------------------------------------
         langtag
         ;; --------------------------------------
         "./xmlnames.rkt")

(provide (contract-out
          (make-xml-declaration
           (->* ((or/c "1.0" "1.1")) (#:encoding (or/c symbol? #f) #:standalone (or/c 'yes 'no #f)) p-i?))
          (make-xmlns-attribute (->* (string?) (#:prefix (or/c xml-ncname? #f)) attribute?))
          (attribute-name=? (-> attribute? symbol? boolean?))
          (attribute-name/ns=? (-> attribute? symbol? string? boolean?))
          (attribute-name/optional-ns=? (-> attribute? symbol? string? boolean?))
          (element-name=? (-> element? symbol? boolean?))
          (element-name/ns=? (-> element? symbol? string? boolean?))
          (make-lang-attribute (-> language-tag? attribute?))
          (boolean->xml (-> boolean? string?))))


;; -------------------------------------------------------------------------------------------------
;; Prologue Helpers
;; -------------------------------------------------------------------------------------------------

(define (make-xml-declaration version #:encoding (encoding #f) #:standalone (standalone #f))
  (make-p-i
   #f #f
   'xml
   (string-append
    (format "version=\"~a\"" version)
    (if encoding
        (format " encoding=\"~a\"" encoding)
        "")
    (if standalone
        (format " standalone=\"~a\"" standalone)
        ""))))

;; -------------------------------------------------------------------------------------------------
;; Namespace Helpers
;; -------------------------------------------------------------------------------------------------

(define (make-xmlns-attribute namespace #:prefix (prefix #f))
  (make-attribute
   #f #f
   (if prefix (string->symbol (format "xmlns:~a" prefix)) 'xmlns)
   namespace))

(define (attribute-name=? att name)
  (eq? (attribute-name att) name))

(define (attribute-name/ns=? att name namespace)
  (let ((ns (attribute-namespace att)))
    (if (and ns (string=? ns namespace))
        (attribute-name=? att name)
        #f)))

(define (attribute-name/optional-ns=? att name namespace)
  (let ((ns (attribute-namespace att)))
    (if (or (false? ns) (string=? ns namespace))
        (attribute-name=? att name)
        #f)))

(define (element-name=? elt name)
  (eq? (element-name elt) name))

(define (element-name/ns=? elt name namespace)
  (let ((ns (element-namespace elt)))
    (if (and ns (string=? ns namespace))
        (element-name=? elt name)
        #f)))

;; -------------------------------------------------------------------------------------------------
;; Lang/Base/Space Helpers
;; -------------------------------------------------------------------------------------------------

(define (make-lang-attribute language)
  (make-attribute
   #f #f
   'xml:lang
   language))

;; -------------------------------------------------------------------------------------------------
;; Datatype Helpers
;; -------------------------------------------------------------------------------------------------

(define (boolean->xml v)
  (if v "true" "false"))
