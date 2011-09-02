\name{dcmle-methods}
\alias{dcmle-methods}
\alias{show,dcMle-method}
\alias{summary,dcMle-method}
\alias{vcov,dcMle-method}
\alias{quantile,dcMle-method}
\alias{dcdiag,dcMle-method}
\alias{dctable,dcMle-method}
\alias{coef,dcMle-method}
\alias{dcsd,dcMle-method}
\alias{confint,dcMle-method}
\alias{nclones,dcMle-method}
\title{
Methods for 'dcMle' Objects
}
\description{
Methods for \code{\link{dcMle}} objects.
}
\details{
Methods defined in the \pkg{dclone} package 
can be used on \code{\link{dcMle}} objects as well.
}
\value{
Specific methods return values as described on their original help pages.

The show and summary method displays Wald-type summaries 
and data cloning diagnostics.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
Underlying S3 methods:
\code{\link{coef.mcmc.list}}, \code{\link{dcsd.mcmc.list}}, 
\code{\link{quantile.mcmc.list}},
\code{\link{vcov.mcmc.list.dc}}, \code{\link{confint.mcmc.list.dc}}, 
\code{\link{dctable}}, \code{\link{dcdiag}}, \code{\link{nclones}}.
}
\examples{
## Data and model taken from Ponciano et al. 2009
## Ecology 90, 356-362.
paurelia <- c(17,29,39,63,185,258,267,392,510,570,650,560,575,650,550,480,520,500)
paramecium <- new("dcFit")
paramecium@data <- list(ncl=1, n=length(paurelia), Y=dcdim(data.matrix(paurelia)))
paramecium@model <- custommodel(beverton.holt <- function() {
    for (k in 1:ncl) {
        for(i in 2:(n+1)){
            Y[(i-1), k] ~ dpois(exp(X[i, k])) # observations
            X[i, k] ~ dnorm(mu[i, k], 1 / sigma^2) # state
            mu[i, k] <- X[(i-1), k] + log(lambda) - log(1 + beta * exp(X[(i-1), k]))
        }
        X[1, k] ~ dnorm(mu0, 1 / sigma^2) # state at t0
    }
    beta ~ dlnorm(-1, 1) # Priors on model parameters
    sigma ~ dlnorm(0, 1)
    tmp ~ dlnorm(0, 1)
    lambda <- tmp + 1
    mu0 <- log(2)  + log(lambda) - log(1 + beta * 2)
})
paramecium@multiply <- "ncl"
paramecium@unchanged <- "n"
paramecium@params <- c("lambda","beta","sigma")
\dontrun{
m <- dcmle(paramecium, n.clones=2, n.iter=1000)
m
coef(m)
dcsd(m)
confint(m)
vcov(m)
quantile(m, probs=c(0.05, 0.95))
dctable(m)
dcdiag(m)
nclones(m)
}
}
\keyword{ methods }
