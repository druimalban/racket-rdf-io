#lang scribble/manual

@(require racket/sandbox
          scribble/core
          scribble/eval
          (for-label rdf/core/name
                     rdf/core/namespace
                     rdf/core/nsmap
                     rdf/io/nquads
                     rdf/io/ntriples
                     rdf/io/trix
                     rdf/io/xml))

@;{============================================================================}

@(define example-eval
   (make-base-eval '(require rdf/io/base
                             rdf/io/registry)))

@;{============================================================================}
@title[#:version  "0.1.0"]{RDF I/O Representations}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]

TBD.

@table-of-contents[]

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module base}
@defmodule[rdf/io/base]

TBD

@defstruct*[representation (
            (id symbol?)
            (name string?)
            (file-extensions (listof string?))
            (reader (or/c reader/c #f))
            (writer writer?))]{
TBD
}

@;{============================================================================}
@subsection[]{Reading}

@defthing[reader/c contract/c]{

@itemlist[
  @item{@racket[statement-list?] --}
  @item{@racket[graph?] --}
  @item{@racket[dataset?] --}
]
}
(define reader/c (-> input-port? (or/c statement-list? graph? dataset?)))

@;{============================================================================}
@subsection[]{Writing}

@defstruct*[writer (
            (dataset (->* (dataset?) (output-port? #:map nsmap?) void?))
            (graph (->* (graph?) (output-port? #:map nsmap?) void?))
            (statement (->* (statement?) (output-port? #:map nsmap?) void?))
            (literal (->* (literal?) (output-port? #:map nsmap?) void?)))]{
TBD
}


@;{============================================================================}
@subsection[]{Exceptions}


@defstruct*[(exn:fail:representation exn:fail) ()
            #:extra-constructor-name make-exn:fail:representation]{
TBD
}

@defthing[representation-operation/c flat-contract?]{
TBD

@itemlist[
  @item{@racket['read] -- (see @racket[raise-representation-read-error])}
  @item{@racket['write] -- (see @racket[raise-representation-write-error])}
]
}

@defproc[(raise-representation-error
          [repr-id symbol?]
          [op representation-operation/c]
          [message string?])
         any]{
TBD
}

@defproc[(raise-representation-read-error
          [repr-id symbol?]
          [expecting string?]
          [found string?]
          [context (or/c string? (listof (cons/c string? string?))) '()])
         any]{
TBD
}

@defproc[(raise-representation-write-error
          [repr-id symbol?]
          [component string?]
          [context (or/c string? (listof (cons/c string? string?))) '()])
         any]{
TBD
}


@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module registry}
@defmodule[rdf/io/registry]

TBD


@defproc[(register-representation
          [repr representation?])
         void?]{
TBD
}

@defproc[(unregister-representation
          [repr-id symbol?])
         void?]{
TBD
}

@defproc[(has-representation?
          [repr-id symbol?])
          boolean?]{
TBD

@examples[
  #:eval example-eval
  (has-representation? 'n-triples)
  (has-representation? 'ntriples)
]
}

@defproc[(get-representation
          [repr-id symbol?])
          (or/c representation? #f)]{
TBD

@examples[
  #:eval example-eval
  (get-representation 'n-triples)
  (get-representation 'ntriples)
]
}

@defproc[(get-registered-representations)
         (listof symbol?)]{
TBD

@examples[
  #:eval example-eval
  (get-registered-representations)
]
}

@subsection[]{Helper Functions}

@defstruct*[(exn:fail:representation:unknown exn:fail:representation) ()
            #:extra-constructor-name make-exn:fail:representation:unknown]{
TBD
}

@defproc[(representation-read
          [repr-id symbol?]
          [inport input-port? (current-input-port)])
         (or/c statement-list? graph? dataset?)]{
TBD
}

@defproc[(representation-write-dataset
          [repr-id symbol?]
          [dataset dataset?]
          [out output-port? (current-output-port)]
          [#:map nsmap nsmap? (make-rdf-only-nsmap)])
         void?]{
TBD
}

@defproc[(representation-write-graph
          [repr-id symbol?]
          [graph graph?]
          [out output-port? (current-output-port)]
          [#:map nsmap nsmap? (make-rdf-only-nsmap)])
         void?]{
TBD
}

@defproc[(representation-write-statement
          [repr-id symbol?]
          [statement statement?]
          [out output-port? (current-output-port)]
          [#:map nsmap nsmap? (make-rdf-only-nsmap)])
         void?]{
TBD
}

@defproc[(representation-write-literal
          [repr-id symbol?]
          [literal literal?]
          [out output-port? (current-output-port)]
          [#:map nsmap nsmap? (make-rdf-only-nsmap)])
         void?]{
TBD
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module nquads}
@defmodule[rdf/io/nquads]

TBD

See the specification at @cite["RDF11NQ"].

@defthing[nquad-representation representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/nquads)
  nquad-representation
]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module ntriples}
@defmodule[rdf/io/ntriples]

TBD

See the specification at @cite["RDF11NT"].

@defthing[ntriple-representation representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/ntriples)
  ntriple-representation
]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module trig}
@defmodule[rdf/io/trig]

TBD

See the specification at @cite["RDF11TRIG"].

@defthing[trig-representation representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/trig)
  trig-representation
]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module trix}
@defmodule[rdf/io/trix]

TBD

See the specification at @cite["RDF11TRIX"].

@defthing[trix-representation representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/trix)
  trix-representation
]
}

@defthing[trix-representation/w3c representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/trix)
  trix-representation/w3c
]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module turtle}
@defmodule[rdf/io/turtle]

TBD

See the specification at ["RDF11TTL"].

@defthing[turtle-representation representation?]{
TBD

@; @examples[
@;   #:eval example-eval
@;   (require rdf/io/turtle)
@;   turtle-representation
@; ]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module xml}
@defmodule[rdf/io/xml]

TBD

See the specification at @cite["RDF11XML"].

@defthing[xml-representation representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/xml)
  xml-representation
]
}

@;{============================================================================}
@;{============================================================================}

@(bibliography

  (bib-entry #:key "RDF11JSON"
             #:title "RDF 1.1 JSON Alternate Serialization (RDF/JSON)"
             #:author "I. Davis, T. Steiner, and A. J Le Hors"
             #:location "W3C"
             #:url "https://www.w3.org/TR/rdf-json/"
             #:date "7 November 2013")

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
