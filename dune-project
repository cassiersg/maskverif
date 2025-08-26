(lang dune 3.6)
(using menhir 2.0)

(name maskverif)

(generate_opam_files true)

(package
  (name maskverif)
  (depends
    menhir
    ocamlgraph
    yojson
    zarith))
