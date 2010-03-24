\encoding{UTF-8}
\name{ovenb}
\alias{ovenb}
\docType{data}
\title{ Ovenbird Occurres }
\description{
  Ovenbird occurrences, ABMI data.
}
\usage{data(rdata)}
\format{
  A data frame with 1027 observations of  6 variables.
}
\details{
\code{oven}: Numeric, observed Ovenbird occurrences.
\code{uplow}: Factor w/ 2 levels "upland","lowland", ecological site classification.
\code{precip}: Factor w/ 2 levels "0","1", precipitation.
\code{wind}: Numeric (scaled and centered), wind speed.
\code{hour}: Numeric (scaled and centered), hour of observation.
\code{jday}: Numeric (scaled and centered), Julian day.
}
\examples{
data(ovenb)
str(ovenb)
}
\keyword{datasets}
