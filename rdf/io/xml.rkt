#lang racket/base

(require racket/bool
         racket/contract
         racket/function
         racket/list
         racket/string
         ;; --------------------------------------
         "../name.rkt"
         "./strings.rkt")


(define/contract (ncname? val)
  ;; [4]    NCName          ::= Name - (Char* ':' Char*)
  ;;                            /* An XML Name, minus the ":" */
  (-> any/c boolean?)
  (cond
    ((symbol? val)
     (ncname? (symbol->string val)))
    ((non-empty-string? val)
     (let ((char-list (string->list val)))
       (and (name-start-char? (car char-list))
            (andmap name-char? (cdr char-list)))))
    (else #f)))

(define/contract (string->ncname str)
  (-> string? (or/c string? #f))
  (if (ncname?) str #f))

(define/contract (symbol->ncname sym)
  (-> symbol? (or/c string? #f))
  (string->ncname (symbol->string sym)))

(define/contract (qname? val)
  ;; [7]    QName           ::= PrefixedName | UnprefixedName
  ;; [8]    PrefixedName    ::= Prefix ':' LocalPart
  ;; [9]    UnprefixedName  ::= LocalPart
  ;; [10]   Prefix          ::= NCName
  ;; [11]   LocalPart       ::= NCName
  ;;
  ;;        QName           ==> (Prefix ':')? LocalPart
  ;;                        ==> (NCName ':')? NCName
  (-> any/c boolean?)
  (cond
    ((symbol? val) (qname? (symbol->string val)))
    ((string? val)
     (let ((split (string-split val name-separator #:trim? #f #:repeat? #f)))
       (or (and (= (length split) 1)
                ;; UnprefixedName
                (ncname? (car split)))
           (and (= (length split) 2)
                ;; PrefixedName
                (ncname? (car split))
                (ncname? (cadr split))))))
    (else #f)))

(define (name-start-char? c)
  ;; [4]    NameStartChar   ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6]
  ;;                          | [#xD8-#xF6]
  ;;                          | [#xF8-#x2FF] | [#x370-#x37D]
  ;;                          | [#x37F-#x1FFF]| [#x200C-#x200D]
  ;;                          | [#x2070-#x218F] | [#x2C00-#x2FEF]
  ;;                          | [#x3001-#xD7FF] | [#xF900-#xFDCF]
  ;;                          | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
  (or
   ;; remove the ":" as per XML namespaces
   (char=? c #\_)
   (char-between-any?
    `(,range-ascii-upper
      ,range-ascii-lower
      (#\u00C0 . #\u00D6)
      (#\u00D8 . #\u00F6)
      (#\u00F8 . #\u02FF)
      (#\u0370 . #\u037D)
      (#\u037F . #\u1FFF)
      (#\u200C . #\u200D)
      (#\u2070 . #\u218F)
      (#\u2C00 . #\u2FEF)
      (#\u3001 . #\uD7FF)
      (#\uF900 . #\uFDCF)
      (#\uFDF0 . #\uFFFD)
      (#\U10000 . #\UEFFF9))
    c)))

(define (name-char? c)
  ;; [4a]   NameChar        ::= NameStartChar | "-" | "." | [0-9] | #xB7
  ;;                          | [#x0300-#x036F] | [#x203F-#x2040]
  (or
   (name-start-char? c)
   (char=? c #\-)
   (char=? c #\.)
   (char=? c #\u00B7)
   (char-between-any?
    `(,range-ascii-digit
      (#\U0300 . #\U036F)
      (#\U203F . #\U2040))
    c)))

(struct xml-pname
  (prefix name)
  #:constructor-name internal-make-xml-pname
  #:methods gen:prefixed-name
  ((define (get-prefix pname) (xml-pname-prefix pname))
   (define (get-name pname) (xml-pname-name pname))
   (define (prefix-valid? _ s) (ncname? s))
   (define (name-valid? _ s) (ncname? s))
   ))

(define/contract (make-xml-pname prefix name)
  (-> (or/c ncname? #f) ncname? xml-pname?)
  (internal-make-xml-pname prefix name))
