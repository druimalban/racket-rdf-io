#lang info
(define collection 'multi)
(define deps '("base" "rdf-core"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("rdf/io/scribblings/rdf-io.scrbl" (multi-page) (library))))
(define test-omit-paths '("rdf/io/scribblings"))
(define license 'Apache-2.0)
