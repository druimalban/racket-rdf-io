#lang racket/base

(require racket/contract
         racket/string
         ;; --------------------------------------
         rdf/core/dataset
         rdf/core/graph
         rdf/core/literal
         rdf/core/nsmap
         rdf/core/statement
         ;; --------------------------------------
         media-type)

(provide reader/c
         dataset-writer/c
         graph-writer/c
         (struct-out representation)
         ;; --------------------------------------
         (struct-out exn:fail:representation)
         make-exn:fail:representation
         representation-operation/c
         raise-representation-error
         raise-representation-read-error
         raise-representation-write-error)

;; -------------------------------------------------------------------------------------------------
;; Structures
;; -------------------------------------------------------------------------------------------------

(define reader/c (->* () (input-port? #:map nsmap?) (or/c statement-list? graph? dataset?)))

(define dataset-writer/c (->* (dataset?) (output-port? #:map nsmap?) void?))

(define graph-writer/c (->* (graph?) (output-port? #:map nsmap?) void?))

;; -------------------------------------------------------------------------------------------------

(struct representation (id name mime-type file-extensions reader dataset-writer graph-writer)
  #:transparent
  #:guard (struct-guard/c symbol?
                          string?
                          (or/c media-type? #f)
                          (listof string?)
                          (or/c reader/c #f)
                          (or/c dataset-writer/c #f)
                          (or/c graph-writer/c #f)))

;; -------------------------------------------------------------------------------------------------
;; Exceptions
;; -------------------------------------------------------------------------------------------------

(struct exn:fail:representation exn:fail ()
  #:extra-constructor-name make-exn:fail:representation
  #:transparent)

(define representation-operation/c (or/c 'read 'write))

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
