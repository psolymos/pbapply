\name{coef.dcglm}
\alias{coef.dcglm}
\alias{confint.dcglm}
\alias{fitted.dcglm}
\alias{logLik.dcglm}
\alias{print.dcglm}
\alias{summary.dcglm}
\alias{vcov.dcglm}
\alias{print.summary.dcglm}
\title{
S3 Methods for 'dcglm' Objects
}
\description{
S3 methods for 'dcglm' objects.
}
\usage{
\method{coef}{dcglm}(object, ...)
\method{confint}{dcglm}(object, parm, level = 0.95, ...)
\method{fitted}{dcglm}(object, ...)
\method{logLik}{dcglm}(object, ...)
\method{print}{dcglm}(x, digits = max(3, getOption("digits") - 3), ...)
\method{print}{summary.dcglm}(x, 
digits = max(3, getOption("digits") - 3), 
signif.stars = getOption("show.signif.stars"), ...)
\method{summary}{dcglm}(object, ...)
\method{vcov}{dcglm}(object, ...)
}
\arguments{
  \item{object, x}{
an fitted model object ofclass 'dcglm'.
}
  \item{parm}{
a specification of which parameters are to be given
confidence intervals, either a vector of numbers or a vector
of names.  If missing, all parameters are considered.
}
  \item{level}{
the confidence level required.
}
\item{digits}{
minimal number of significant digits.
}
\item{signif.stars}{
if significance starts should be showed.
}
  \item{\dots}{
other arguments.
}
}
\value{
Coefficients extracted from the model object \code{object}.
}
\author{
Peter Solymos \email{solymos@ulaberta.ca}
}
\keyword{ htest }
