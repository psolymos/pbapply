\encoding{UTF-8}
\name{singleocc.fit}
\Rdversion{1.1}
\alias{singleocc.fit}
\title{
Internal Function to Fit a Single Visit ZIB Model
}
\description{
This functions is used internally by \code{\link{singleocc}}.
It is normally not called directly but can be more efficient where the response vector and design mattrices
have already been calculated.
}
\usage{
singleocc.fit(obs, occ, det, link.occ = "logit", link.det = "logit", penalized = FALSE, auc = FALSE,
method = c("optim", "dc"), n.clones = 1000, prec = 0.1, ...)
}
\arguments{
  \item{obs}{
Observations.
}
  \item{occ}{
Model matrix correponding to the occurrence model part.
}
  \item{det}{
Model matrix correponding to the detection model part.
}
  \item{link.occ}{
Character, link function to be used for the occurrence model part. Can be any of \code{c("logit", "probit", "cloglog")}.
}
  \item{link.det}{
Character, link function to be used for the detection model part. Can be any of \code{c("logit", "probit", "cloglog")}.
}
  \item{penalized}{
Logical, if the penalized maximum likelihood estimate should be calculated.
}
  \item{auc}{
Logical, if area under the curve (AUC, as returned by \code{\link{AUC}}) should be part of the returned value.
}
  \item{method}{
Numerical optimization method to be used. See Details.
}
  \item{n.clones}{
Number of clones, if \code{method = "dc"}.
}
  \item{prec}{
Precision for Normal prior distributions (means are taken from naive GLMs), if \code{method = "dc"}.
}
  \item{\dots}{
Other arguments passed to the functions (e.g. MCMC settings passed to function \code{\link[dclone]{jags.fit}}).
}
}
\value{
A fitted model object (but without a class attribute).
}
\references{
Lele, S.R., M. Moreno and E. Bayne, 2010.
Dealing with detection error in site occupancy surveys: What can we do with a single survey?
\enmph{Methods in Ecology and Evolution} \strong{00}, 00--00.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos} and Monica Moreno
}
\seealso{
Easy model fitting: \code{\link{singleocc}}
}
\keyword{ htest }
\keyword{ models }
