.packageName = "allegRo"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to allegRo, the R package to interact with AllegroGraph. \n To start working with RDF data and AllegroGraph, install the latest version of AllegroGraph at http://franz.com/agraph/downloads/")
}
