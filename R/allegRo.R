.packageName = "allegRo"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to allegRo, the R package to interact with AllegroGraph. To start working with RDF data and AllegroGraph, install a free server at http://franz.com/agraph/downloads/")
}
