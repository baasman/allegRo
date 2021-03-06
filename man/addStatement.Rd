% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statement_funcs.R
\name{addStatement}
\alias{addStatement}
\title{Add a statement}
\usage{
addStatement(repository, subj = NULL, pred = NULL, obj = NULL,
  context = NULL)
}
\arguments{
\item{repository}{Object of type repository specifying server details and repository to work on.}

\item{subj}{valid url}

\item{pred}{valid url}

\item{obj}{valid url}

\item{context}{context of triple}
}
\value{
Return: successful push or not
}
\description{
Add a triple to the repository.
The subj, pred, and obj arguments must be supplied and are used to fill in the subject, predicate and object parts of the triple. The context argument is used to fill in the triple's graph but may be left off. In this case, the triple's graph will be the default graph of the repository.

The subj, pred, obj, and context (if supplied) arguments must all be in URI encoded N-Triples notation.

The service returns the triple-ID of the newly added triple.
}
\examples{
\dontrun{
service = createService("localhost","user","password")
rep = repository(catalog(service,"root"),"test")
subj = '<www.test.com/tmp#person>'
pred = '<www.test.com/tmp#hasItem>'
obj= '<www.test.com/tmp#sword>'
addStatement(rep,
subj = subj,pred = pred,obj = obj)
}
}

