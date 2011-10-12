\name{hsarx}
\alias{hsarx}
\alias{hsarx-classes}
\alias{show,hsarx-method}
\title{
Fit SAR, SARX, HSAR and HSARX models to data
}
\description{
Fit SAR, SARX, HSAR and HSARX models to data
}
\usage{
hsarx(formula, data, n.clones, cl = NULL, subset, na.action, ...)
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
  \item{subset}{
Subset to data argument.
}
  \item{na.action}{
Handling of NA values.
}
  \item{\dots}{
Other arguments for MCMC.
}
}
\details{
Fit SAR, SARX, HSAR and HSARX models to data as described in Solymos and Lele
}
\value{
An mcmc.list object.
}
\references{
Solymos, P. and Lele, S. R. (in press), Global pattern and local variation
in species-area relationships. 
Global Ecology and Biogeography, xx: xx--xx.
}
\author{
Peter Solymos
}
\seealso{
jags.fit, etc.
}
\examples{
\dontrun{
## to reproduce results from Solymos and Lele (Table 1)
data(sardata)
DAT <- data.frame(sardata$islands, sardata$studies[match(sardata$islands$study, rownames(sardata$studies)),])
x <- hsarx(log(S+0.5) ~ log(A) | (taxon.group + island.type + abs(latitude) + I(log(extent)))^2 | study, DAT, 
    n.clones=5, n.adapt=2000, n.update=3000, n.iter=1000)

## SAR
DATS <- DAT[1:191,]
x1 <- hsarx(log(S+0.5) ~ log(A), DATS[DATS$study=="abbott1978bird",], n.clones=2)

## SARX
DATS$rnd <- rnorm(nrow(DATS), log(DATS$extent))
x2 <- hsarx(log(S+0.5) ~ log(A) * rnd, DATS[DATS$study=="abbott1978bird",], n.clones=2)

## HSAR
x3 <- hsarx(log(S+0.5) ~ log(A) | 1 | study, DATS, n.clones=2, n.iter=1000)

## HSARX
x4 <- hsarx(log(S+0.5) ~ log(A) | abs(latitude) | study, DATS, n.clones=2, n.iter=1000)
}
}
\keyword{ htest }
\keyword{ models }
