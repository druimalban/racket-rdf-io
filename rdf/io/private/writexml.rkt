#lang racket/base

(require racket/bool
         racket/list
         racket/string
         ;; --------------------------------------
         "./formatter.rkt")

(provide (all-defined-out))

(define xml-formatter (make-parameter #f #f 'xml-formatter))

(define (newline)
  (let ((fmt (xml-formatter)))
    (if (false? fmt) "" "\n")))

(define (xml:namespace uri (prefix #f))
  (cons (format "xmlns~a" (if prefix (string-append ":" prefix) "")) uri))

(define (xml:base uri)
  (cons "xml:base" uri))

(define (xml:lang uri)
  (cons "xml:lang" uri))

(define (format-attribute key value)
  (format "~a=~s" key value))

(define (attribute-padding fmt attributes name-len)
  (if (false? fmt)
      " "
      (let ((sum (+ (for/sum ((s attributes)) (string-length s)) name-len)))
        (if (> sum (formatter-line-width fmt))
            (string-append "\n" (indentation-string fmt))
            " "))))

(define (format-attributes attributes elname)
  (let* ((fmt (xml-formatter))
         (attrs (map (λ (key+value) (format-attribute (car key+value) (cdr key+value)))
                     attributes))
         (padding (attribute-padding fmt attrs (+ (string-length elname) 2))))
    (string-append padding (string-join attrs padding))))

(define (prolog-entry name attributes)
  (let ((fmt (xml-formatter)))
    (format "<?~a~a?>~a"
            name
            (format-attributes attributes name)
            (newline))))

(define (standard-prolog-entry (version "1.0") (encoding #f) (standalone #f))
  (let ((attributes (list (cons 'version version))))
    (when encoding (cons (cons 'encoding encoding) attributes))
    (when standalone (cons (cons 'standalone standalone) attributes))
    (prolog-entry "xml" attributes)))

(define (element-content content #:newline (end-line #t))
  ;; TODO: ensure correct escaping.
  (let* ((fmt (xml-formatter))
         (sol (if (false? fmt) "" (indentation-string fmt)))
          (eol (if end-line (newline) "")))
    (format "~a~a~a" sol content eol)))

(define (element-start name #:attrs (attributes '()) #:newline (end-line #t))
  (let* ((fmt (xml-formatter))
         (sol (if (false? fmt) "" (indentation-string fmt)))
         (eol (if end-line (newline) "")))
    (unless (false? fmt)
      (indent fmt))
    (format "~a<~a~a>~a" sol name (format-attributes attributes name) eol)))

(define (element-end name (fmt #f) #:newline (end-line #t))
  (let ((fmt (xml-formatter)))
    (unless (false? fmt)
      (outdent fmt))
    (let ((sol (if (false? fmt) "" (indentation-string fmt)))
          (eol (if end-line (newline) "")))
      (format "~a</~a>~a" sol name eol))))

(define (element-start+end name #:attrs (attributes '()))
  (let ((fmt (xml-formatter)))
    (unless (false? fmt)
      (indent fmt))
    (format "<~a~a/>" name (format-attributes attributes name))))

(define (element name #:attrs (attributes '())  #:content (content #f))
  (let ((fmt (xml-formatter)))
    (cond ((and (empty? attributes) (false? content))
         (element-start+end name))
        ((false? content)
         (string-append
          (element-start name #:attrs attributes #:newline #f)
          (element-end name #:newline #f)))
        (else
         (string-append
          (element-start name #:attrs attributes)
          (element-content content)
          (element-end name))))))
