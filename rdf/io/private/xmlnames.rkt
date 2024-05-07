#lang racket/base

(require racket/bool
         racket/contract
         racket/string
         rx)

(provide (contract-out
         (xml-ncname? (-> any/c boolean?))
         (xml-qname? (-> any/c boolean?))
         (xml-split-qname (-> non-empty-string? (cons/c (or xml-ncname? #f) xml-ncname?)))))


;; -------------------------------------------------------------------------------------------------
;; From https://www.w3.org/TR/xml11/ Section 2.3, Common Syntactic Constructs
;;
;; [4]    NameStartChar   ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6]
;;                          | [#xD8-#xF6]
;;                          | [#xF8-#x2FF] | [#x370-#x37D]
;;                          | [#x37F-#x1FFF]| [#x200C-#x200D]
;;                          | [#x2070-#x218F] | [#x2C00-#x2FEF]
;;                          | [#x3001-#xD7FF] | [#xF900-#xFDCF]
;;                          | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
;; [4a]   NameChar        ::= NameStartChar | "-" | "." | [0-9] | #xB7
;;                          | [#x0300-#x036F] | [#x203F-#x2040]
;; [5]    Name            ::= NameStartChar (NameChar)*
;;
;; -------------------------------------------------------------------------------------------------

(define name-start-char
  (rx/match
   (rx/range #\A #\Z)
   #\_
   (rx/range #\a #\z)
   (rx/range #\u00C0 #\u00D6)
   (rx/range #\u00D8 #\u00F6)
   (rx/range #\u00F8 #\u02FF)
   (rx/range #\u0370 #\u037D)
   (rx/range #\u037F #\u1FFF)
   (rx/range #\u200C #\u200D)
   (rx/range #\u2070 #\u218F)
   (rx/range #\u2C00 #\u2FEF)
   (rx/range #\u3001 #\uD7FF)
   (rx/range #\uF900 #\uFDCF)
   (rx/range #\uFDF0 #\uFFFD)
   (rx/range #\U10000 #\UEFFF9)))

(define name-char
  (rx/nc-group
   (rx/or
    name-start-char
    (rx/match
     #\-
     #\.
     (rx/range #\0 #\9)
     #\uB7
     (rx/range #\U0300 #\U036F)
     (rx/range #\U203F #\U2040)))))

;; -------------------------------------------------------------------------------------------------
;; From https://www.w3.org/TR/xml-names/ Section 3, Declaring Namespaces
;;
;; [4]    NCName          ::= Name - (Char* ':' Char*)
;;                            /* An XML Name, minus the ":" */
;;
;; -------------------------------------------------------------------------------------------------

(define ncname/str (rx/group name-start-char (rx/* name-char)))
(define ncname (regexp (rx/string-exactly ncname/str)))

(define (xml-ncname? v)
  (and (string? v)
       (not (false? (regexp-match ncname v)))))

;; -------------------------------------------------------------------------------------------------
;; From https://www.w3.org/TR/xml-names/ Section 4, Qualified Names
;;
;; [7]    QName           ::= PrefixedName | UnprefixedName
;; [8]    PrefixedName    ::= Prefix ':' LocalPart
;; [9]    UnprefixedName  ::= LocalPart
;; [10]   Prefix          ::= NCName
;; [11]   LocalPart       ::= NCName
;;
;;        QName           ==> (Prefix ':')? LocalPart
;;                        ==> (NCName ':')? NCName
;;
;; -------------------------------------------------------------------------------------------------

(define qname/str
  (rx/nc-group
   (rx/group ncname/str ":" #:repeat 'optional)
   ncname/str))

(define qname (regexp qname/str))

(define (xml-qname? v)
  (and (string? v)
       (not (false? (regexp-match qname v)))))

(define (xml-split-qname qname)
  (let ((prefix+local (string-split qname ":")))
    (cond
      ((= (length prefix+local) 1)
       (let ((local-part (car prefix+local)))
         (if (xml-ncname? local-part)
           (cons #f local-part)
           (raise-argument-error 'local-part "ncname?" local-part))))
      ((= (length prefix+local) 2)
       (let ((prefix (car prefix+local))
             (local-part (cadr prefix+local)))
         (cond
          ((not (xml-ncname? prefix))
           (raise-argument-error 'prefix "ncname?" prefix))
          ((not (xml-ncname? local-part))
           (raise-argument-error 'local-part "ncname?" local-part))
          (else (cons prefix local-part)))))
      (else (raise-argument-error 'qname "qname?" qname)))))
