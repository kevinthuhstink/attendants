(lang dune 3.20)

(name attendants)

(generate_opam_files true)

(source
 (github kevinthuhstink/attendants))

(authors "Kevin Cheng")

(package
 (name attendants)
 (depends ocaml cohttp-lwt-unix lwt yojson)
 )
