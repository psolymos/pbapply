\name{linkinvbinom}
\alias{linkinvbinom}
\title{ Common Inverse Link Functions for Binary Responses }

\description{
  The function \code{linkinvbinom} is a convenient function to switch
  the inverse link function for the Binomial cases.
}

\usage{
linkinvbinom(link)
}
\arguments{
  \item{link}{ Caharcter, one of \code{"logit"}, \code{"probit"},
    \code{"loglog"}, \code{"cloglog"}. }
}


\details{
  The function \code{linkinvbinom} handles the link argument within
  the community generating function for Binomial generalized models.
}

\value{
  A function.
}
\author{ Peter Solymos }
\examples{
x <- rnorm(10)
x
FUN <- linkinvbinom("logit")
FUN(x)
all.equal(FUN(x), exp(x) / (1 + exp(x)))
}
\keyword{ manip }
