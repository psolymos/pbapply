\name{model.select}
\alias{model.select}
\alias{pva.llr}
\alias{print.pvaModelSelect}
\title{
Model selection for 'pva' objects
}
\description{
Likelihood ratio calculation and model selection
for (hierarchical) 'pva' objects.

\code{pva.llr} is the workhorse behind 
\code{model.select}. \code{pva.llr} can also
be used for profile likelihood calculations
if called iteratively (no wrapper presently).
}
\usage{
pva.llr(null, alt, pred)
model.select(null, alt, B = 10^3)
\method{print}{pvaModelSelect}(x, ...)
}
\arguments{
  \item{null}{
A fitted 'pva' object representing the
Null Hypothesis.
}
  \item{alt}{
A fitted 'pva' object representing the
Alternative Hypothesis (broader model).
}
  \item{B}{
Number of replicates to be generated from
the latent variables.
}
  \item{pred}{
A matrix of replicates from the latent variables,
e.g. es returned by \code{\link{predict.latent}}.
}
  \item{x}{
A model selection object to be printed.
}
  \item{\dots}{
Additional argument fr print method.
}
}
\details{
FIXME
}
\value{
\code{pva.llr} returns a single numeric value, the 
log likelihood ratio of the two models (logLR0 - logLR1).

\code{model.select} returns a modified data frame
with log likelihood ration and various information
criteria metrics (AIC, BIC, AICc).

The print method gives fancy model names and
a human readable interpretation of the numbers.
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
m1 <- pva(songsparrow, gompertz("none"), 2, n.iter=1000)
m2 <- pva(songsparrow, gompertz("poisson"), 2, n.iter=1000)
m3 <- pva(songsparrow, gompertz("normal"), 2, n.iter=1000)
p <- predict.latent(m2, n.chains=1, n.iter=10000)
pva.lrtest(m1, m2, p)
model.select(m1, m2)
model.select(m1, m3)
model.select(m2, m3)

m1x <- pva(songsparrow, ricker("none"), 2, n.iter=1000)
m2x <- pva(songsparrow, ricker("poisson"), 2, n.iter=1000)
m3x <- pva(songsparrow, ricker("normal"), 2, n.iter=1000)

model.select(m1, m1x)
model.select(m2, m2x)
model.select(m3, m3x)

model.select(m1x, m2x)
model.select(m1x, m3x)
model.select(m2x, m3x)

## profile likelihood
m <- pva(songsparrow, gompertz("normal"), 5, n.iter=5000)
p <- predictLatent(m, n.chains=1, n.iter=10000)
m1 <- pva(songsparrow, gompertz("normal", fixed=c(sigma=0.4)), 5, n.iter=5000)
## etc for many sigma values
pva.llr(m1, m, p) # calculate log LR for each
## finally, fit smoother to points and plot
}
}
\keyword{ htest }
\keyword{ models }
\keyword{ ts }
