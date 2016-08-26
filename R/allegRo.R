#' allegRo: A package for dealing with RDF data and AllegroGraph
#'
#' This package interacts with AllegroGraph through a REST interface,
#' and provides many of the common actions one may want to execute in AllegroGraph.
#' In addition to this package, Python, Java, Lisp and Ruby clients all exist, but working
#' with AG from R allows the user to seamlessly integrate advanced analytics into their RDF
#' Database.
#'
#' @section allegRo functions:
#' allegRo functions...
#'
#' @docType package
#' @name allegRo
NULL



.packageName = "allegRo"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to allegRo, the R package to interact with AllegroGraph. \nTo start working with RDF data and AllegroGraph, install the latest version of AllegroGraph at http://franz.com/agraph/downloads/")
}
