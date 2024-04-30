Racket Package rdf-io
=======
A more complete set of RDF I/O procedures than the rdf-core package.

[![raco pkg install rdf-io](https://img.shields.io/badge/raco%20pkg%20install-rdf--io-blue.svg)](http://pkgs.racket-lang.org/package/rdf-dc)
[![Documentation](https://img.shields.io/badge/raco%20docs-rdf--io-blue.svg)](http://docs.racket-lang.org/rdf-dc/index.html)
[![Racket](https://github.com/johnstonskj/racket-rdf-io/actions/workflows/racket.yml/badge.svg)](https://github.com/johnstonskj/racket-rdf-dc/actions/workflows/racket.yml)
[![GitHub release](https://img.shields.io/github/release/johnstonskj/racket-rdf-io.svg?style=flat-square)](https://github.com/johnstonskj/racket-rdf-dc/releases)
[![GitHub stars](https://img.shields.io/github/stars/johnstonskj/racket-rdf-io.svg)](https://github.com/johnstonskj/racket-rdf-dc/stargazers)

## Status

| Pri. | Representation | Writer      | Reader      | Comments                                  |
|------|----------------|-------------|-------------|-------------------------------------------|
| 1    | N-Triples      | Complete    | Not Started | Line-Oriented, triples only.              |
| 1    | Turtle         | Started     | Not Started |                                           |
| 2    | RDF/XML        | Started     | Not Started | Part of the RDF core specifications.      |
| 2    | N-Quads        | Complete    | Not Started | Effectively N-Triples + Graphs            |
| 2    | TriG           | Started     | Not Started | Effectively Turtle + Graphs               |
| 3    | RDF/JSON       | Not Started | Not Started |                                           |
| 3    | TriX           | Complete    | Not Started |                                           |
| 4    | JSON-LD        | Not Started | Not Started |                                           |
| 4    | Notation-3     | Not Started | Not Started | Superset of Turtle with non-RDF features. |
| 5    | OWL Functional | Not Started | Not Started | Specific for OWL.                         |
| 5    | OWL Manchester | Not Started | Not Started | Specific for OWL.                         |

## Example

TBD

```racket
(define int-42 (make-typed-literal "42" (string->url "http://www.w3.org/2001/XMLSchema#integer")))

(define *test-graph*
  (named-graph
   (string->url "http://example.com/peeps")
   (statement-list "http://example.com/p/me"
                   (list (list "http://example.com/v/people#hasFirstName" (list (make-lang-string-literal "Me" "en")))
                         (list "http://example.com/v/people#hasLastName" (list "!"))
                         (list "http://example.com/v/people#hasScores" (list 2 4 int-42))))))

(define (display-graph-as graph repr)
  (let* ((representation (get-representation repr))
         (writer (representation-writer representation))
         (graph-writer (writer-graph writer)))
    (graph-writer graph)))

(display-graph-as *test-graph* 'trix)
```

## Changes

**Version 0.1.0**

Initial release.
