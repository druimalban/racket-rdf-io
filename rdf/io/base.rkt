#lang racket/base

(require racket/contract
         racket/string
         ;; --------------------------------------
         rdf/core/graph
         rdf/core/dataset
         rdf/core/statement
         rdf/core/literal)

(provide reader/c
         (struct-out writer)
         (struct-out representation)
         ;; --------------------------------------
         (struct-out exn:fail:representation)
         make-exn:fail:representation
         raise-representation-read-error
         raise-representation-write-error)

;; -------------------------------------------------------------------------------------------------
;; Structures
;; -------------------------------------------------------------------------------------------------

(define reader/c (-> input-port? (or/c statement-list? graph? dataset?)))

;; -------------------------------------------------------------------------------------------------

(struct writer (dataset graph statement literal)
  #:transparent
  #:guard (struct-guard/c (->* (dataset?) (output-port?) void?)
                          (->* (graph?) (output-port?) void?)
                          (->* (statement?) (output-port?) void?)
                          (->* (literal?) (output-port?) void?)))

;; -------------------------------------------------------------------------------------------------

(struct representation (id name file-extensions reader writer)
  #:transparent
  #:guard (struct-guard/c symbol?
                          string?
                          (listof string?)
                          (or/c reader/c #f)
                          writer?))

;; -------------------------------------------------------------------------------------------------
;; Exceptions
;; -------------------------------------------------------------------------------------------------

(struct exn:fail:representation exn:fail ()
  #:extra-constructor-name make-exn:fail:representation
  #:transparent)

(define (raise-representation-error repr op message)
  (raise (make-exn:fail:representation
          (format "~a: ~a error\n~a" repr op message)
          (current-continuation-marks))))

(define (format-context-string ctx)
  (if ctx
      (format "  context...:\n    ~a"
              (string-join
               (map (λ (item)
                      (cond ((string? item) item)
                            ((pair? item) (format "~a: ~a" (car item) (cdr item)))
                            (else (format "~s" item)))) ctx)
               "\n    "))
      ""))

;; -------------------------------------------------------------------------------------------------

(define (raise-representation-read-error repr expecting found (context '()))
  (raise-representation-error
   repr
   'read
   (format "  expecting: ~a\n  found: ~a\n~a"
           expecting
           found
           (format-context-string context))))

;; -------------------------------------------------------------------------------------------------

(define (raise-representation-write-error repr component (context '()))
  (raise-representation-error
   repr
   'write
   (format "  writing: ~a\n~a"
           component
           (format-context-string context))))


