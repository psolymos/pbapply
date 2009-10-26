\encoding{UTF-8}
\name{jags.fit}
\Rdversion{1.1}
\alias{jags.fit}
\alias{bugs.fit}
\alias{as.mcmc.list.bugs}
\title{
Fit JAGS/BUGS models with cloned data
}
\description{
Convenient functions designed to work well with cloned data arguments.
}
\usage{
jags.fit(data, params, model, inits = NULL, n.chains = 3, n.adapt = 1000,
n.update = 0, thin = 1, n.iter = 5000, ...)
bugs.fit(data, params, model, inits = NULL, format = c("mcmc.list", "bugs"), 
program = c("winbugs", "openbugs"), DIC = FALSE, dir = getwd(), ...)
\method{as.mcmc.list}{bugs}(x, ...)
}
\arguments{
  \item{data}{
A list containing the data.
}
  \item{params}{
Character vector of parameters to be samples.
}
  \item{model}{
Character string (name of the model file) or a function containing the model (see Examples).
}
  \item{inits}{
Optional specification of initial values in the form of a list or a function (see Initialization at \code{\link[rjags]{jags.model}}). 
If \code{NULL}, initial values will be generated automatically. It is an error to supply an initial value for an observed node.
}
  \item{n.chains}{
Number of chains to generate.
}
  \item{n.adapt}{
Number of steps for adaptation.
}
  \item{n.update}{
Number of updates before iterations.
}
  \item{thin}{
Thinning value.
}
  \item{n.iter}{
Number of iterations.
}
  \item{format}{
Required output format. (Note that default behaviour of \code{n.thin} can cause problems when \code{program = "openbugs"}
and \code{format = "mcmc.list"}, thus \code{n.thin = 1} is advised.)
}
  \item{program}{
The program to use, not case sensitive.
\code{winbugs} calls the function \code{\link[R2WinBUGS]{bugs}},
\code{openbugs} calls the function \code{\link[R2WinBUGS]{openbugs}} and requires the CRAN package \pkg{BRugs}.
\code{model} can be a function for \code{program = "openbugs"}, too.
}
  \item{DIC}{
Logical, if deviance information criterion should be computed. (Needed for efficiency reasons, since it is
opposite to the \code{bugs}/\code{openbugs} defaults, but if \code{format = "mcmc.list"} deviance is
silently ignored.)
}
  \item{dir}{
Identical to \code{working.directory} argument in the \code{bugs}/\code{openbugs} call for setting working directory.
(Needed for stability, because \code{openbugs} cant't always handle its default \code{NULL} value (a bug?).)
}
  \item{x}{
A fitted 'bugs' object.
}
  \item{\dots}{
Further arguments passed to \code{\link[rjags]{coda.samples}}, and \code{\link[rjags]{update.jags}}
(e.g. the \code{progress.bar} argument) for \code{jags.fit}. For \code{bugs.fit}, other arguments
except for \code{codaPkg} are passed also, most notably the ones to set up burn-in, thin, etc. (see Details).
}
}
\value{
By default, an \code{mcmc.list} object. If data cloning is used via the 
\code{data} argument, \code{summary(jags.fit)} returns a modified summary
containing scaled data cloning standard errors (scaled by \code{sqrt(n.clones)}),
and \eqn{R_{hat}} values (as returned by \code{\link[coda]{gelman.diag}}).

\code{bugs.fit} returns a \code{bugs} object if \code{format = "bugs"}.
In this case, printing is not changed, but the number of clones used is attached as attribute
and can be retrieved by the function \code{\link{nclones}}.

The function \code{as.mcmc.list.bugs} converts a 'bugs' object into 'mcmc.list'.
Default behaviour of \code{n.thin} can cause problems in conversion when \code{program = "openbugs"}
and \code{format = "mcmc.list"}, thus \code{n.thin = 1} is advised.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link[rjags]{jags.model}}, \code{\link[rjags]{coda.samples}}

\code{\link{dcsd}}, \code{\link{confint.mcmc.list.dc}}, 
\code{\link{coef.mcmc.list}}, \code{\link{quantile.mcmc.list}}, \code{\link{vcov.mcmc.list.dc}}
}
\examples{
\dontrun{
## simple regression example from the JAGS manual
jfun <- function() {
    for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], tau)
        mu[i] <- alpha + beta * (x[i] - x.bar)
    }
    x.bar <- mean(x)
    alpha ~ dnorm(0.0, 1.0E-4)
    beta ~ dnorm(0.0, 1.0E-4)
    sigma <- 1.0/sqrt(tau)
    tau ~ dgamma(1.0E-3, 1.0E-3)
}
## data generation
set.seed(1234)
N <- 100
alpha <- 1
beta <- -1
sigma <- 0.5
x <- runif(N)
linpred <- model.matrix(~x) %*% c(alpha, beta)
Y <- rnorm(N, mean = linpred, sd = sigma)
## list of data for the model
jdata <- list(N = N, Y = Y, x = x)
## what to monitor
jpara <- c("alpha", "beta", "sigma")
## fit the model
regmod <- jags.fit(jdata, jpara, jfun, n.chains = 3)
## model summary
summary(regmod)
## data cloning
dcdata <- dclone(jdata, 5, multiply = "N")
dcmod <- jags.fit(dcdata, jpara, jfun, n.chains = 3)
summary(dcmod)
## time series example
## data and model taken from Ponciano et al. 2009
## Ecology 90, 356-362.
beverton.holt <- function() {
    for (k in 1:ncl) {
        for(i in 2:(n+1)){
            ## observations
            Y[(i-1), k] ~ dpois(exp(X[i, k]))
            ## state
            X[i, k] ~ dnorm(mu[i, k], 1 / sigma^2)
            mu[i, k] <- X[(i-1), k] + log(lambda) - log(1 + beta * exp(X[(i-1), k]))
        }
        ## state at t0
        X[1, k] ~ dnorm(mu0, 1 / sigma^2)
    }
    # Priors on model parameters
    beta ~ dlnorm(-1, 1)
    sigma ~ dlnorm(0, 1)
    tmp ~ dlnorm(0, 1)
    lambda <- tmp + 1
    mu0 <- log(2)  + log(lambda) - log(1 + beta * 2)
}
paurelia <- c(17,29,39,63,185,258,267,392,510,570,650,560,575,650,550,480,520,500)
dat <- list(ncl=1, n=length(paurelia), Y=as.ts(paurelia))
dcdat <- dclone(dat, 10, multiply="ncl", unchanged="n")
mod <- jags.fit(dcdat, c("lambda","beta","sigma"), beverton.holt)
## compare with results from the paper:
##   beta   = 0.00235
##   lambda = 2.274
##   sigma  = 0.1274
summary(mod)
}
}
\keyword{ models }

