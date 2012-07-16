\name{generateMissing}
\alias{generateMissing}
\title{
Generate missing value
}
\description{
Generate missing value of a time series.
}
\usage{
generateMissing(x, ...)
}
\arguments{
  \item{x}{
A fitted PVA model object.
}
  \item{\dots}{
Arguments passed to \code{\link[dclone]{jags.fit}},
such as \code{n.iter}, \code{n.chains}.
}
}
\details{
It uses MLE from fitted PVA model
to generate missing values.
}
\value{
A marix with \code{n.iter * n.chains} rows and as
many columns as the number of missing values in the 
time series.
}
\references{
Ponciano, J. M. et al. 2009. Hierarchical models in ecology: 
confidence intervals, hypothesis testing, 
and model selection using data cloning. Ecology 90, 356-362.
}
\author{
Khurram Nadeem and Peter Solymos
}
\seealso{
\code{\link{pva}}
}
\examples{
\dontrun{
data(paurelia)
m <- pva(paurelia, gompertz("normal"), 5)
p <- generateMissing(m, n.chains=1, n.iter=1000)
summary(p)
}
}
\keyword{ utils }
\keyword{ models }
