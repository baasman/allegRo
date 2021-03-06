% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repo_funcs.R
\name{addNameSpace}
\alias{addNameSpace}
\title{Add a namespace to the repository}
\usage{
addNameSpace(repository, prefix, nsURI)
}
\arguments{
\item{repository}{Object of type repository specifying server details and repository to work on.}

\item{prefix}{Prefix}

\item{nsURI}{Namespace URI}
}
\value{
ag put object that states whether or not push was successful (invisible)
}
\description{
Add a namespace for the current user in this repository
}
\examples{
\dontrun{
service = service("localhost","user","password")
rep = repository(catalog(service,"root"),"test")
addNameSpace(service,catalogid = "root",repo = "testRepo",prefix = "foaf",
nsURI = "http://xmlns.com/foaf/0.1/")
}
}

