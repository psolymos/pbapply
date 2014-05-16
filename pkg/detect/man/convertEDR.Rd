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
Matsuoka, S. M., Bayne, E. M., Solymos, P., Fontaine, P., 
Cumming, S. G., Schmiegelow, F. K. A., & Song, S. A., 2012.
Using binomial distance-sampling models to estimate the effective 
detection radius of point-counts surveys across boreal Canada. 
\emph{Auk}, 129, 268--282.

Solymos, P., Matsuoka, S. M., Bayne, E. M., Lele, S. R., Fontaine, P., 
Cumming, S. G., Stralberg, D., Schmiegelow, F. K. A. & Song, S. J., 2013.
Calibrating indices of avian density from non-standardized survey data: 
making the most of a messy situation. 
\emph{Methods in Ecology and Evolution}, 4: 1047--1058.

Supporting info, including a tutorial for the above paper:
\url{http://dcr.r-forge.r-project.org/qpad/}
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
