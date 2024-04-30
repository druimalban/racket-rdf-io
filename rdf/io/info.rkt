#lang info
(define name "rdf/io: RDF serialization support")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/rdf-core.scrbl" ())))
(define pkg-desc "Serialization support for RDF graphs.")
(define version "0.1.0")
(define pkg-authors '(johnstonskj))
(define license '(Apache-2.0))
