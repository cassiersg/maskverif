with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "mask-verif";
  src = ./.;
  buildInputs = [ ]
    ++ (with ocamlPackages; [ ocaml findlib menhir menhirLib zarith merlin ocamlgraph yojson])
    ++ [dune_3]
    ;
}
