\encoding{UTF-8}
\name{ovenbird}
\Rdversion{1.1}
\alias{ovenbird}
\docType{data}
\title{
Abundances of ovenbird in Alberta
}
\description{
The data set contains observations of 310 sites of the Alberta Biodiversity Monitoring Institute.

\code{count}: integer, ovenbird counts per site.

\code{ecosite}: factor with 5 levels, ecological categorisation of the sites.

\code{disturb.succ, disturb.alien}: numeric, percentage of successional and alienating human disturbance
within 500 m radius of the site location.

\code{long, lat}: numeric, public longitute/latitude coordinates of the sites.
}
\usage{data(coralheads)}
\source{
Alberta Biodiversity Monitoring Institute, \url{http://www.abmi.ca}
}
\examples{
\dontrun{
data(ovenbird)
summary(ovenbird)
str(ovenbird)
}
}
\keyword{datasets}
