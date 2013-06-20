\name{hsarx}
\alias{hsarx}
\alias{hsarx-class}
\alias{show,hsarx-method}
\title{
Fit SAR, SARX, HSAR and HSARX models to data
}
\description{
Fit SAR, SARX, HSAR and HSARX models to data as
described in Solymos and Lele (2012).
}
\usage{
hsarx(formula, data, n.clones, cl = NULL, ...)
}
\arguments{
  \item{formula}{
Formula.
}
  \item{data}{
Data.
}
  \item{n.clones}{
Number of clones to be used.
}
  \item{cl}{
Cluster object for parallel computations.
}
  \item{\dots}{
Other arguments for MCMC.
}
}
\details{
Fit SAR, SARX, HSAR and HSARX models to data as described in 
Solymos and Lele (2012).
}
\value{
An S4 object object of class 'hsarx'. It inherits from 'dcMle',
and has additional slots for storing the data.
}
\references{
Solymos, P. and Lele, S. R., 2012. 
Global pattern and local variation in species-area relationships. 
\emph{Global Ecology and Biogeography} 21, 109--120.
}
\author{
Peter Solymos
}
\seealso{
\code{\link{sardata}} for data sets.
}
\examples{
\dontrun{
## to reproduce results from Solymos and Lele (Table 1)
data(sardata)
DAT <- data.frame(sardata$islands, 
    sardata$studies[match(sardata$islands$study, 
    rownames(sardata$studies)),])
x <- hsarx(log(S+0.5) ~ log(A) | (taxon.group + island.type + 
    abs(latitude) + I(log(extent)))^2 | study, DAT, 
    n.clones=5, n.adapt=2000, n.update=3000, n.iter=1000)

## SAR
DATS <- DAT[1:191,]
(x1 <- hsarx(log(S+0.5) ~ log(A), 
    DATS[DATS$study=="abbott1978bird",], n.clones=2))

## SARX
DATS$rnd <- rnorm(nrow(DATS), log(DATS$extent))
(x2 <- hsarx(log(S+0.5) ~ log(A) * rnd, 
    DATS[DATS$study=="abbott1978bird",], n.clones=2))

## HSAR
(x3 <- hsarx(log(S+0.5) ~ log(A) | 1 | study, 
    DATS, n.clones=2, n.iter=1000))

## HSARX
(x4 <- hsarx(log(S+0.5) ~ log(A) | abs(latitude) | study, 
    DATS, n.clones=2, n.iter=1000))
}
}
\keyword{ htest }
\keyword{ models }
