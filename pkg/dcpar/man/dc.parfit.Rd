\name{dc.parfit}
\Rdversion{1.1}
\alias{dc.parfit}
\title{
Iterative model fitting with data cloning
}
\description{
The iterative model fitting in parallel.
}
\usage{
dc.parfit(cl, data, params, model, inits, n.clones, 
multiply=NULL, unchanged=NULL, 
update=NULL, updatefun=NULL, initsfun=NULL, 
trace=1, flavour = c("jags", "bugs"), ...)
}
\arguments{
  \item{cl}{
A cluster object created by \code{\link[snow]{makeCluster}}.
}
  \item{data}{
A named list (not environment) containing the data.
}
  \item{params}{
Character vector of parameters to be samples.
}
  \item{model}{
Character string (name of the model file) or a function containing the model (see Examples).
}
  \item{inits}{
Optional specification of initial values in the form of a list or a function (see Initialization at \code{\link[rjags]{jags.model}}). 
If missing, will be treated as \code{NULL} and initial values will be generated automatically.
}
  \item{n.clones}{
An integer vector containing the numbers of clones to use itaratively.
}
  \item{multiply}{
Numeric or character index for list element(s) in the \code{data} argument
to be multiplied by the number of clones instead of repetitions.
}
  \item{unchanged}{
Numeric or character index for list element(s) in the \code{data} argument
to be left unchanged.
}
  \item{update}{
Numeric or character index for list element(s) in the \code{data} argument
that has to be updated by \code{updatefun} in each iterations. This usually
is for making priors more informative, and anhancing convergence.
See Details and Examples.
}
  \item{updatefun}{
A function to use for updating \code{data[[update]]}. It should take an 'mcmc.list' object as argument.
See Details and Examples.
}
  \item{initsfun}{
A function to use for generating initial values, \code{inits} are updated by the object
returned by this function from the second iteration. If initial values
are not dependent on the previous iteration, this should be \code{NULL}, otherwise,
it should take an 'mcmc.list' object as argument. See Examples.
}
  \item{trace}{
  If positive, information is printed during the running of the iterations.
  Higher number indicate more information.
}
  \item{flavour}{
  If \code{"jags"}, the function \code{\link[dclone]{jags.fit}} is called. If \code{"bugs"}, the function \code{\link[dclone]{bugs.fit}} is called.
}
  \item{\dots}{
  Other values supplied to \code{\link[dclone]{jags.fit}}, or \code{\link[dclone]{bugs.fit}}, depending on the \code{flavour} argument.
}
}
\details{
The function fits a JAGS/BUGS model with increasing numbers of clones, as supplied by
the argument \code{k}. Data cloning is done by the function \code{dclone} using
the arguments \code{multiply} and \code{unchanged}.
An updating function can be provided, see Examples.

The \code{dc.parfit} os a parallel computing version of \code{\link[dclone]{dc.fit}}.
}
\value{
An object inheriting from the class 'mcmc.list'.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}, 
implementation is based on many discussions with Khurram Nadeem and Subhash Lele.
}
\seealso{
Data cloning: \code{\link[dclone]{dclone}}. \

Model fitting: \code{\link[dclone]{jags.fit}}, \code{\link[dclone]{bugs.fit}}

Convergence diagnostics: \code{\link[dclone]{dctable}}
}
\examples{
\dontrun{
## time series example
## data and model taken from Ponciano et al. 2009
## Ecology 90, 356-362.
paurelia <- c(17,29,39,63,185,258,267,392,510,570,650,560,575,650,550,480,520,500)
dat <- list(ncl=1, n=length(paurelia), Y=as.ts(paurelia))
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
mod <- dc.fit(dat, c("lambda","beta","sigma"), beverton.holt,
    n.clones=c(2, 5, 10), multiply="ncl", unchanged="n")
## compare with results from the paper:
##   beta   = 0.00235
##   lambda = 2.274
##   sigma  = 0.1274
summary(mod)
## parallel computation
cl <- makeSOCKcluster(2)
pmod <- dc.parfit(cl, dat, c("lambda","beta","sigma"), beverton.holt,
    n.clones=c(2, 5, 10), multiply="ncl", unchanged="n")
summary(pmod)
stopCluster(cl)
}
}
\keyword{ models }
\keyword{ htest }
