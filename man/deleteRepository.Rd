% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/service functions.R
\name{deleteRepository}
\alias{deleteRepository}
\title{Delete a Repository}
\usage{
deleteRepository(service, catalogid = "root", repositoryid = "new_test2")
}
\arguments{
\item{service}{Service object containing service url, username, and password}

\item{catalogid}{Id for catalog of interest}

\item{repositoryid}{Id for repository of interest}
}
\value{
True or False, denoting whether delete was successful
}
\description{
Delete a Repository
}
