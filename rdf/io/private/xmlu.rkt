#lang racket/base

(require racket/contract
         ;; --------------------------------------
         xml
         ;; --------------------------------------
         langtag
         ;; --------------------------------------
         "./xmlnames.rkt")

(provide (contract-out
          (make-xml-declaration
           (->* ((or/c "1.0" "1.1")) (#:encoding (or/c symbol? #f) #:standalone (or/c 'yes 'no #f)) p-i?))
          (make-xmlns-attribute (->* (string?) (#:prefix (or/c xml-ncname? #f)) attribute?))
          (make-lang-attribute (-> language-tag? attribute?))
          (boolean->xml (-> boolean? string?))))

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

(define (make-xmlns-attribute namespace #:prefix (prefix #f))
  (make-attribute
   #f #f
   (if prefix (string->symbol (format "xmlns:~a" prefix)) 'xmlns)
   namespace))

(define (make-lang-attribute language)
  (make-attribute
   #f #f
   'xml:lang
   language))

(define (boolean->xml v)
  (if v "true" "false"))
