#lang scribble/manual

@(require racket/sandbox
          scribble/core
          scribble/eval
          rdf/dc/terms
          rdf/dc/elements
          rdf/dc/type
          rdf/dc/cam
          (for-label rdf/core/name
                     rdf/core/namespace
                     rdf/core/nsmap
                     rdf/io/nquads
                     rdf/io/ntriples
                     rdf/io/trix
                     rdf/io/xml))

@;{============================================================================}
@title[#:version  "0.1.0"]{RDF I/O Representations}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]

TBD.

@tabular[
]

@table-of-contents[]

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module base}
@defmodule[rdf/io/base]

TBD

;;(provide reader/c
;;         (struct-out writer)
;;         (struct-out representation)
;;         (struct-out exn:fail:representation)
;;         make-exn:fail:representation
;;         raise-representation-read-error
;;         raise-representation-write-error)


@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module registry}
@defmodule[rdf/io/registry]

TBD

;;(provide register-representation
;;         get-representation
;;         get-known-representations)


@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module nquads}
@defmodule[rdf/io/registry]

TBD

@defthing[nquad-representation representation?]{
TBD
}

See @cite["RDF11NQ"].

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module ntriples}
@defmodule[rdf/io/registry]

TBD

@defthing[ntriple-representation representation?]{
TBD
}

See @cite["RDF11NT"].

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module trig}
@defmodule[rdf/io/registry]

TBD

@cite["RDF11TRIG"].

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module trix}
@defmodule[rdf/io/trix]

TBD

@defthing[trix-representation representation?]{
TBD
}

See @cite["RDF11TTL"].

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module turtle}
@defmodule[rdf/io/registry]

TBD

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module xml}
@defmodule[rdf/io/registry]

TBD

@;{============================================================================}
@;{============================================================================}

@(bibliography

  (bib-entry #:key "RDF11NQ"
             #:title "RDF 1.1 N-Quads -- A line-based syntax for RDF datasets"
             #:author "G. Carothers"
             #:location "W3C"
             #:url "https://www.w3.org/TR/n-quads/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11NT"
             #:title "RDF 1.1 N-Triples -- A line-based syntax for an RDF graph"
             #:author "P. J. Hayes, and P. F. Patel-Schneider"
             #:location "W3C"
             #:url "https://www.w3.org/TR/n-triples/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11TRIG"
             #:title "RDF 1.1 TriG: RDF Dataset Language"
             #:author "G. Carothers, A. Seaborne, C. Bizer, and R. Cyganiak"
             #:location "W3C"
             #:url "https://www.w3.org/TR/trig/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11TRIX"
             #:title "TriX: An XML Serialization for RDF Triples"
             #:author "J. J. Carroll, and P. Stickler"
             #:location "Nokia"
             #:url "https://web.archive.org/web/20110724134923/http://sw.nokia.com/trix/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11TTL"
             #:title "RDF 1.1 Turtle -- Terse RDF Triple Language"
             #:author "D. Beckett, T. Berners-Lee, E. Prud'hommeaux, and . Carothers"
             #:location "W3C"
             #:url "https://www.w3.org/TR/turtle/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11XML"
             #:title "RDF 1.1 XML Syntax"
             #:author "F. Gandon, and G. Schreiber"
             #:location "W3C"
             #:url "https://www.w3.org/TR/rdf-syntax-grammar/"
             #:date "25 February 2014")

)

@;{============================================================================}
@;{============================================================================}
@index-section[]
