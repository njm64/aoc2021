(lang dune 2.9)
(name aoc)

(generate_opam_files true)

(package
 (name aoc)
 (depends
  (ocaml (>= 4.13.0))
  (base  (>= 0.14.1))
  (ppx_let (>= 0.14.0))
  (ppx_compare (>= 0.14.0)))
 (synopsis "Advent of Code 2021"))
