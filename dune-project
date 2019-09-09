(lang dune 1.11)
(using fmt 1.2)
(generate_opam_files true)
(name gr)

(source (github mseri/ocaml-gr))
(license ISC)
(authors "Marcello Seri")
(maintainers "Marcello Seri")

(package
 (name gr)
 (synopsis "OCaml bindings to the GR plotting library")
 (description "OCaml bindings to the GR plotting library")
 (depends
  (dune (> 1.11))
  (ctypes (>= 0.15))
  (ctypes-foreign (>= 0.15))
  (base-unix :with-test)))
