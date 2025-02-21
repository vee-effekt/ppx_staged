(lang dune 3.6)

(generate_opam_files true)

(name ppx_staged)

(source
 (github johnyob/ppx-template))

(license MIT)

(authors "Thia Richey")

(maintainers "lapwing@seas.upenn.edu")

(package
 (name ppx_staged)
 (depends
  ocaml
  (dune
   (>= 3.6))
  (odoc :with-doc)
  core
  fast_gen
  ppx_jane
  ppx_assert
  base
  stdio
  fast_gen
  base_quickcheck
  ppx_deriving
  ppxlib
  alcotest
   (and
    :with-test)
  core_unix
  core))
