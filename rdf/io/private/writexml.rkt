#lang racket/base

(require racket/bool
         racket/list
         racket/string)

(provide (all-defined-out))

(define (xml:namespace uri (prefix #f))
  (cons (format "xmlns~a" (if prefix (string-append ":" prefix) "")) uri))

(define (xml:base uri)
  (cons "xml:base" uri))

(define (xml:lang uri)
  (cons "xml:lang" uri))

(define (format-attribute key value)
  (format " ~a=~s" key value))

(define (format-attributes attributes)
  (apply string-append
   (map (λ (key+value) (format-attribute (car key+value) (cdr key+value)))
        attributes)))

(define (prolog-entry name attributes)
  (format "<?~a~a?>" name (format-attributes attributes)))

(define (standard-prolog-entry (version "1.0") (encoding #f) (standalone #f))
  (let ((attributes (list (cons 'version version))))
    (when encoding (cons (cons 'encoding encoding) attributes))
    (when standalone (cons (cons 'standalone standalone) attributes))
    (prolog-entry "xml" attributes)))

(define (element-content content)
  ;; TODO: ensure correct escaping.
  (format "~a" content))

(define (element-start name (attributes '()))
  (format "<~a~a>" name (format-attributes attributes)))

(define (element-end name)
  (format "</~a>" name))

(define (element-start+end name (attributes '()))
  (format "<~a~a/>" name (format-attributes attributes)))

(define (element name #:attrs (attributes '())  #:content (content #f))
  (if (and (empty? attributes) (false? content))
      (element-start+end name)
      (string-append
        (element-start name attributes)
        (if (not (false? content))
            (element-content content)
            "")
        (element-end name))))
