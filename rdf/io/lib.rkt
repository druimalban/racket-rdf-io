#lang racket/base

(require racket/bool
         racket/contract
         net/url-structs
         "../core/graph.rkt")

(provide (struct-out serialization)
         make-serialization
         register-serialization
         get-serialization
         get-known-serializations
         write-as)

(struct serialization (id name file-extensions spec writer reader)
  #:sealed
  #:constructor-name make-serialization
  #:guard (struct-guard/c symbol? string? (listof string?) url? procedure? (or/c void? procedure?)))

(define *known-serializations* (make-hash))

(define/contract (register-serialization obj)
  (-> serialization? void?)
  (hash-set! *known-serializations* (serialization-id obj) obj))

(define/contract (get-serialization id)
  (-> symbol? serialization?)
  (hash-ref *known-serializations* id))

(define/contract (get-known-serializations)
  (-> (listof (list/c symbol? string?)))
  (map (λ (ser) (list (serialization-id ser) (serialization-name ser)))
       (hash-values *known-serializations*)))

(define/contract (write-as serialization graph (out (current-output-port)))
  (-> symbol? graph? output-port?)
  (let ((serialization (get-serialization serialization)))
    (if (not (false? serialization))
        ((serialization-writer serialization) graph out)
        (error))))
