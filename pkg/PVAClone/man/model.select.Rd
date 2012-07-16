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
model.select(null, alt, B = 10^4)
\method{print}{pvaModelSelect}(x, ...)
}
\arguments{
  \item{null}{
A fitted 'pva' object representing the
Null Hypothesis.
}
  \item{alt}{
A fitted 'pva' object representing the
Alternative Hypothesis (usually broader model).
}
  \item{B}{
Number of replicates to be generated from
the latent variables.
}
  \item{pred}{
A matrix of replicates from the latent variables,
e.g. as returned by \code{\link{generateLatent}}.
}
  \item{x}{
A model selection object to be printed.
}
  \item{\dots}{
Additional argument for print method.
}
}
\details{

These functions implement Ponciano et. al.'s (2009) data cloning
likelihood ratio algorithm (DCLR)
to compute likelihood ratios for comparing hierarchical 
(random effect) models. In the population growth 
models context, these models are (1) with observation error 
population growth models, and/or 
(2) population growth models with missing observations. 

The functions can also compute likelihood ratios 
when both of the population growth models are fixed effect models, 
e.g. without observation error Gompertz 
model Vs. without observation error Ricker model. 
See examples below and in \code{\link{pva}}.

}
\value{
\code{pva.llr} returns a single numeric value, the 
log likelihood ratio of the two models (logLik0 - logLik1).

\code{model.select} returns a modified data frame
with log likelihood ratio and various information
criteria metrics (delta AIC, BIC, AICc).

The print method gives fancy model names and
a human readable interpretation of the numbers.
}
\references{

Ponciano, J. M. et al. 2009. Hierarchical models in ecology: 
confidence intervals, hypothesis testing, 
and model selection using data cloning. Ecology 90, 356-362. 

Nadeem, K., Lele S. R., 2012.  Likelihood based population 
viability analysis in the presence of 
observation error. Oikos. doi: 10.1111/j.1600-0706.2011.20010.x

}
\author{
Khurram Nadeem and Peter Solymos
}
\seealso{
\code{\link{pva}}
}
\examples{
\dontrun{
data(redstart)
m1 <- pva(redstart, gompertz("none"), 2, n.iter=1000)
m2 <- pva(redstart, gompertz("poisson"), 2, n.iter=1000)
m3 <- pva(redstart, gompertz("normal"), 2, n.iter=1000)
p <- generateLatent(m2, n.chains=1, n.iter=10000)
pva.llr(m1, m2, p)
model.select(m1, m2)
model.select(m1, m3)
model.select(m2, m3)

m1x <- pva(redstart, ricker("none"), 2, n.iter=1000)
m2x <- pva(redstart, ricker("poisson"), 2, n.iter=1000)
m3x <- pva(redstart, ricker("normal"), 2, n.iter=1000)

model.select(m1, m1x)
model.select(m2, m2x)
model.select(m3, m3x)

## missing data situation
data(paurelia)
m1z <- pva(paurelia, ricker("none"), 2, n.iter=1000)
m2z <- pva(paurelia, ricker("poisson"), 2, n.iter=1000)
m3z <- pva(paurelia, ricker("normal"), 2, n.iter=1000)

model.select(m1z, m2z)
model.select(m1z, m3z)
model.select(m2z, m3z)

## profile likelihood
m <- pva(redstart, gompertz("normal"), 5, n.iter=5000)
p <- generateLatent(m, n.chains=1, n.iter=10000)
m1 <- pva(redstart, gompertz("normal", 
    fixed=c(sigma=0.4)), 5, n.iter=5000)
## etc for many sigma values
pva.llr(m1, m, p) # calculate log LR for each
## finally, fit smoother to points and plot
}
}
\keyword{ htest }
\keyword{ models }
\keyword{ ts }
