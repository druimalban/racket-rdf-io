#lang racket/base

(require racket/bool
         racket/contract
         ;; --------------------------------------
         rdf/core/dataset
         rdf/core/graph
         rdf/core/statement
         rdf/core/literal
         rdf/core/nsmap
         ;; --------------------------------------
         "./base.rkt"
         "./json.rkt"
         "./nquads.rkt"
         "./ntriples.rkt"
         "./tabular.rkt"
         "./trig.rkt"
         "./trix.rkt"
         "./xml.rkt")

(provide (contract-out
          (register-representation (-> representation? void?))
          (unregister-representation (-> symbol? void?))
          (has-representation? (-> symbol? boolean?))
          (get-representation (-> symbol? (or/c representation? #f)))
          (get-registered-representations (-> (listof symbol?)))
          ;; --------------------------------------
          (representation-read
           (->* (symbol?) (input-port?) (or/c statement-list? graph? dataset? #f)))
          ;; --------------------------------------
          (representation-write-dataset
           (->* (symbol? dataset?) (output-port? #:map nsmap?) void?))
          (representation-write-graph
           (->* (symbol? graph?) (output-port? #:map nsmap?) void?))))

;; -------------------------------------------------------------------------------------------------
;; Structure
;; -------------------------------------------------------------------------------------------------

(define known-representations (make-hash))

(define (register-representation repr)
  (hash-set! known-representations (representation-id repr) repr))

(define (unregister-representation repr-id)
  (hash-remove! known-representations repr-id))

(define (has-representation? repr-id)
  (hash-has-key? known-representations repr-id))

(define (get-representation repr-id)
  (hash-ref known-representations repr-id #f))

(define (get-registered-representations)
  (hash-keys known-representations))

;; -------------------------------------------------------------------------------------------------
;; Helpers
;; -------------------------------------------------------------------------------------------------

(struct exn:fail:representation:unknown exn:fail:representation ()
  #:extra-constructor-name make-exn:fail:representation:unknown
  #:transparent)

(define (representation-read repr-id (inport (current-input-port)))
  (let ((repr (get-representation repr-id)))
    (if (false? repr)
        (raise (make-exn:fail:representation:unknown (symbol->string repr))
               (current-continuation-marks))
        ((representation-reader repr) inport))))

(define (representation-write-dataset
         repr-id
         dataset
         (out (current-output-port))
         #:map (nsmap (make-rdf-only-nsmap)))
  (let ((repr (get-representation repr-id)))
    (if (false? repr)
        (raise (make-exn:fail:representation:unknown (symbol->string repr))
               (current-continuation-marks))
        ((representation-dataset-writer repr) dataset out #:map nsmap))))

(define (representation-write-graph
         repr-id
         graph
         (out (current-output-port))
         #:map (nsmap (make-rdf-only-nsmap)))
  (let ((repr (get-representation repr-id)))
    (if (false? repr)
        (raise (make-exn:fail:representation:unknown (symbol->string repr))
               (current-continuation-marks))
        ((representation-graph-writer repr) graph out #:map nsmap))))

;; -------------------------------------------------------------------------------------------------
;; Initialize registry
;; -------------------------------------------------------------------------------------------------

(register-representation nquad-representation)
(register-representation ntriple-representation)
(register-representation tabular-representation)
(register-representation trig-representation)
(register-representation trix-representation)
(register-representation trix-representation/w3c)
(register-representation json-representation)
(register-representation xml-representation)
