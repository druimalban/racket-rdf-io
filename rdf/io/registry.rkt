#lang racket/base

(require racket/contract
         ;; --------------------------------------
         "./base.rkt"
         "./nquads.rkt"
         "./ntriples.rkt"
         "./trix.rkt")

(provide register-representation
         get-representation
         get-known-representations)

;; -------------------------------------------------------------------------------------------------
;; Structure
;; -------------------------------------------------------------------------------------------------

(define known-representations (make-hash))

(define/contract (register-representation repr)
  (-> representation? void?)
  (hash-set! known-representations (representation-id repr) repr))

(define/contract (get-representation id)
  (-> symbol? representation?)
  (hash-ref known-representations id))

(define/contract (get-known-representations)
  (-> (listof (cons/c symbol? representation?)))
  (map (λ (repr) (cons (representation-id repr) repr))
       (hash-values known-representations)))

(register-representation nquad-representation)
(register-representation ntriple-representation)
(register-representation trix-representation)
(register-representation trix-representation/w3c)
