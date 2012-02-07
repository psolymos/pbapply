\name{predict.missing}
\alias{predict.missing}
\title{
Predict missing value
}
\description{
Predict missing value of a time series.
}
\usage{
predict.missing(x, ...)
}
\arguments{
  \item{x}{
A fitted PVA model object.
}
  \item{\dots}{
Arguments passed to \code{\link{jags.fit}},
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
FIXME
}
\author{
Khurram Nadeem and Peter Solymos
}
\seealso{
\code{\link{pva}}
}
\examples{
\dontrun{
m <- pva(paurelia, gompertz("normal"), 5)
p <- predict.missing(m, n.chains=1, n.iter=1000)
summary(p)
}
}
\keyword{ utils }
\keyword{ models }
