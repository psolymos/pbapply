\name{redstart}
\alias{redstart}
\docType{data}
\title{
Abundance time series of American Redstart
}
\description{
Counts for American Redstart (\emph{Setophaga ruticilla}) at a 
survey location in the North American Breeding Bird Survey 
(BBS; Robbins et al. 1986, Peterjohn 1994).
BBS record number 0214332808636 observed from 1966 to 1995.
}
\usage{data(redstart)}
\format{
  The format is:
 num [1:30] 18 10 9 14 17 14 5 10 9 5 ...
}
\details{
redstart abundance time series
}
\source{
Data reported in B. Dennis,
J. M. Ponciano, S. R. Lele, M. L. Taper, and D. F.
Staples (unpublished manuscript, see Lele 2006).
}
\references{
Lele, S.R. 2006. Sampling variability and estimates of density 
dependence: a composite-likelihood approach.
Ecology 87, 189--202.

Peterjohn, B.G. 1994. 
The North American Breeding Bird Survey. Birding 26, 386--398.

Robbins, C.S., D. Bystrak, and P.H. Geissler. 1986. 
The breeding bird survey: its first
fifteen years, 1965--1979. U.S. Fish and Wildl. Serv. 
Resource Publ. 157. Washington, D.C.
196 pp.
}
\examples{
data(redstart)
redstart
plot(redstart)
}
\keyword{datasets}
