(lang dune 1.11)

(using fmt 1.2)

(generate_opam_files true)

(name gr)

(source
 (github mseri/ocaml-gr))

(license ISC)

(authors "Marcello Seri")

(maintainers "Marcello Seri")

(package
 (name gr)
 (synopsis "OCaml bindings to the GR plotting library")
 (description
  "GR is based on an implementation of a Graphical Kernel System (GKS) and OpenGL. As a self-contained system it can quickly and easily be integrated into existing applications.\nGR is characterized by its high interoperability and can be used with modern web technologies and mobile devices.")
 (depends
  (ocaml
   (>= 4.10))
  (dune
   (>= 1.11))
  (ctypes
   (>= 0.15))
  (ctypes-foreign :true)
  (base-unix :with-test)
  (owl
   (and
    :with-test
    (>= 0.6.0)))))
