\name{goats}
\alias{goats}
\docType{data}
\title{
Mountain Goats Data Set
}
\description{
GPS collar data of mountain goats (\emph{Oreamnos americanus}) from Lele and Keim (2006).
}
\usage{data(goats)}
\format{
  A data frame with 19014 observations on the following 8 variables.
  \describe{
    \item{\code{STATUS}}{a numeric vector, 1: used, 0: available}
    \item{\code{ID}}{a numeric vector, individuals}
    \item{\code{ELEVATION}}{a numeric vector (m)}
    \item{\code{SLOPE}}{a numeric vector (degrees, steep)}
    \item{\code{ET}}{a numeric vector, access to escape terrain (distance from steep slopes, m)}
    \item{\code{ASPECT}}{a numeric vector (degrees)}
    \item{\code{HLI}}{a numeric vector, heat load index (0-1)}
    \item{\code{TASP}}{a numeric vector, transformed aspect}
  }
}
\details{
Mountain goat telemetry data were collected in the
Coast Mountains of northwest British Columbia, Canada,
as described in Lele and Keim (2006).
}
\source{
Ecological Archives E087-181-S1, \url{http://www.esapubs.org/archive/ecol/E087/181/}
}
\references{
Lele, S. R. &  Keim, J. L. (2006) 
Weighted distributions and estimation of resource selection probability functions. 
Ecology 87, 3021--3028.
}
\examples{
data(goats)
str(goats)
summary(goats)

\dontrun{
goats$exp.HLI <- exp(goats$HLI)
goats$sin.SLOPE <- sin(pi * goats$SLOPE / 180)
goats$ELEVATION <- scale(goats$ELEVATION)
goats$ET <- scale(goats$ET)
goats$TASP <- scale(goats$TASP)
summary(m1 <- rspf(STATUS ~ TASP + sin.SLOPE + ELEVATION, goats, B = 99))
summary(m2 <- rspf(STATUS ~ TASP + ELEVATION, goats, B = 99))
AIC(m1, m2)
plot(m1)
}
}
\keyword{datasets}
