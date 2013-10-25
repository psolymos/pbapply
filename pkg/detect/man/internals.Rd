\name{cmulti.fit0}
\alias{cmulti.fit0}
\alias{drop.scope.svisit}
\title{
Internal functions
}
\description{
Internal functions, not intended for use on their own.
}
\usage{
cmulti.fit0(Y, D, type=c("rem", "mix", "dis"), interval=c(-25, 25), ...)
drop.scope.svisit(terms1, terms2, model = c("sta", "det"))
}
\arguments{
  \item{Y}{
this contains the cell counts. See \code{\link{cmulti.fit}}.
}
  \item{D}{
design matrix, that describe the interval endpoints for the sampling 
methodology, dimensions must match dimensions of \code{Y}.
See \code{\link{cmulti.fit}}.
}
  \item{type}{
character, one of \code{"rem"} (removal sampling, homogeneous 
singing rates), \code{"mix"} (removal sampling, heterogeneous 
singing rates), \code{"dis"} (distance sampling, half-normal detection 
function for point counts, circular area). See \code{\link{cmulti.fit}}.
}
  \item{interval}{
the interval used in \code{\link{optimize}}.
}
  \item{terms1}{
the terms or formula for the base model.
}
  \item{terms1}{
the terms or formula for the upper/lower scope. See \code{\link{drop.scope}}.
}
  \item{model}{
character, the type of model to be considered.
}
}
\author{
Peter Solymos
}
\keyword{ internal }
