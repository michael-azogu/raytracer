(lang dune 3.16)

(name raytracer)

(license MIT)

(source
 (github michael-azogu/raytracer))

(package
 (name raytracer)
 (synopsis "a simple raytracer")
 (description "raytracer implementation in ocaml")
 (depends ocaml dune)
 (tags
  (3D raytracer graphics)))

(generate_opam_files true)
; stanza docs https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
