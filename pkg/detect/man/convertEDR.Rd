\name{convertEDT}
\alias{convertEDR}
\title{
Conversion between truncated and unlimited effective detection distance (EDR)
}
\description{
Conversion between truncated and unlimited effective detection distance (EDR).
}
\usage{
convertEDR(edr, r, truncated=FALSE) 
}
\arguments{
  \item{edr}{
effective detection distance. In same units as \code{r}.
}
  \item{r}{
truncation distance (radius of point count). In same units as \code{edr}.
}
  \item{truncated}{
logical, see Details.
}
}
\details{
\code{truncated = FALSE} means that \code{edr} is unlimited
EDR, and the function returns the truncated EDR given \code{r}.

\code{truncated = TRUE} means that \code{edr} is truncated
EDR given \code{r}, and the function returns the unlimited EDR.
}
\value{
A numeric vector with converted EDR values.
}
\references{
Solymos et al. QPAD paper in MEE (under review)
}
\author{
Peter Solymos
}
\examples{
convertEDR(1, 0.5, truncated=FALSE)
## should be close to 1
convertEDR(convertEDR(1, 0.5, truncated=FALSE), 0.5, truncated=TRUE)
}
\keyword{ misc }
