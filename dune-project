(lang dune 2.8)

(name dtools)
(version 0.4.6)
(source (github savonet/ocaml-dtools))
(license GPL-2.0-only)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "Romain Beauxis <toots@rastageeks.org>")

(generate_opam_files true)

(package
 (name dtools)
 (synopsis "Library providing various helper functions to make daemons")
 (depends
  saturn_lockfree
  (ocaml (>= 4.05.0))
  dune)
 (depopts syslog))
