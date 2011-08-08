\name{oven}
\alias{oven}
\docType{data}
\title{
Ovenbird abundances
}
\description{
Ovenbird abundances from BBS
}
\usage{data(oven)}
\format{
  A data frame with 891 observations on the following 11 variables.
  \describe{
    \item{\code{count}}{observations}
    \item{\code{route}}{route id}
    \item{\code{stop}}{stop id within route}
    \item{\code{pforest}}{proportion of forest}
    \item{\code{pdecid}}{proportion of deciduous forest}
    \item{\code{pagri}}{proportion of agricultural areas}
    \item{\code{long}}{longitude}
    \item{\code{lat}}{latitude}
    \item{\code{observ}}{observer, a factor with levels \code{ARS} \code{DW} \code{RDW} \code{SVW}}
    \item{\code{julian}}{Julian day}
    \item{\code{timeday}}{time of day}
  }
}
\source{
BBS
}
\references{
Bayne unpublished
}
\examples{
data(oven)
str(oven)
}
\keyword{datasets}
