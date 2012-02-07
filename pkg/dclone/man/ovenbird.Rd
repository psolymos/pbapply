\encoding{UTF-8}
\name{ovenbird}
\Rdversion{1.1}
\alias{ovenbird}
\docType{data}
\title{
Abundances of ovenbird in Alberta
}
\description{
The data set contains observations (point counts) of 198 sites of the 
Alberta Biodiversity Monitoring Institute.

\code{count}: integer, ovenbird counts per site.

\code{site, year}: numeric, site number and year of data collection.

\code{ecosite}: factor with 5 levels, 
ecological categorisation of the sites.

\code{uplow}: factor with 2 levels, ecological 
categorisation of the sites
(same es ecosite but levels are grouped into 
\code{upland} and \code{lowland}).

\code{dsucc, dalien, thd}: numeric, percentage of successional, 
alienating and total humab disturbance
based on interpreted 3 x 7 km photoplots centered on each site.

\code{long, lat}: numeric, public 
longitute/latitude coordinates of the sites.
}
\usage{data(ovenbird)}
\source{
Alberta Biodiversity Monitoring Institute, \url{http://www.abmi.ca}
}
\examples{
data(ovenbird)
summary(ovenbird)
str(ovenbird)
}
\keyword{datasets}
