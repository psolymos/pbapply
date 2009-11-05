\name{dc.fit}
\Rdversion{1.1}
\alias{dc.fit}
\alias{dctable}
\alias{dctable.default}
\alias{plot.dctable}
\title{
Iterative Algorithm for Testing Data Cloning Convergence
}
\description{
\code{\link{jags.fit}} is iteratively used to fit a model with
increasing the number of clones used until data cloning convergence
is reached.
}
\usage{
dc.fit(data, params, model, inits, n.clones, 
multiply = NULL, unchanged = NULL, 
update = NULL, updatefun = NULL, initsfun = NULL,
trace = 1, flavour = c("jags", "bugs"), ...)
dctable(x, ...)
\method{dctable}{default}(x, ...)
\method{plot}{dctable}(x, which = 1:length(x), 
type = c("all", "sd"),
position = "topright", box.cex = 0.75, ...)
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
A function to use for updating \code{data[[update]]}. It should take and 'mcmc.list' object as argument.
See Details and Examples.
}
  \item{initsfun}{
A function to use for generating initial values, \code{inits} are updated by the object
returned by this function from the second iteration. If initial values
are not dependent on the previous iteration, this should be \code{NULL}, otherwise,
it should take and 'mcmc.list' object as argument. See Examples.
}
  \item{trace}{
  If positive, information is printed during the running of the iterations.
  Higher number indicate more information.
}
  \item{flavour}{
  If \code{"jags"}, the function \code{\link{jags.fit}} is called. If \code{"bugs"}, the function \code{\link{bugs.fit}} is called.
}
  \item{x}{
  An MCMC or a 'dctable' object.
}
  \item{which}{
  What to plot, character names or integer indices of the estimated parameters
  are accepted.
}
  \item{type}{
  Type of plot to be drawn. \code{all} results in plotting means, standard deviations and quantiles
  agains the number of clones as boxplot. \code{sd} reults in plotting the scaled standard deviations
  agains the number of clones. This is possible if there is an uncloned (\code{n.clones = 1}) iteration
  step in the \code{dctable} result. In this case standard deviations are divided by this uncloned
  standard deviation. Along with the values, the 1/\code{n.clones} line is plotted.
}
  \item{position}{
  Position for the legend, as for \code{\link{legend}}.
}
  \item{box.cex}{
  Scaling factor for the interquartile boxes.
}
  \item{\dots}{
  Other values supplied to \code{\link{jags.fit}}, or \code{\link{bugs.fit}}, depending on the \code{flavour} argument.
}
}
\details{
The function fits a JAGS/BUGS model with increasing numbers of clones, as supplied by
the argument \code{k}. Data cloning is done by the function \code{dclone} using
the arguments \code{multiply} and \code{unchanged}.

\code{dctable} returns the \code{"dctable"} attribute of the MCMC object.
This attribute is created during the iterative data cloning process,
to store statistics for the sampled parameters,
because only the last MCMC object is kept. This way, there is a track of
changes in the estimates.

FIXME !!! Add UPDATE and inits details !!! 
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
Khurram Nadeem, \email{knadeem@math.ualberta.ca}

\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
Data cloning: \code{\link{dclone}}

Model fitting: \code{\link{jags.fit}}
}
\examples{
\dontrun{
## simulation for Poisson GLMM
set.seed(1234)
n <- 20
beta <- c(2, -1)
sigma <- 0.1
alpha <- rnorm(n, 0, sigma)
x <- runif(n)
X <- model.matrix(~x)
linpred <- X %*% beta + alpha
Y <- rpois(n, exp(linpred))
## JAGS model as a function
jfun1 <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
    sigma ~ dlnorm(0, 0.001)
}
## data
jdata <- list(n = n, Y = Y, X = X, np = NCOL(X))
## number of clones to be used, etc.
## iteartive fitting
jmod <- dc.fit(jdata, c("beta", "sigma"), jfun1, 
    n.clones = 1:5, multiply = "n", unchanged = "np")
## summary with DC SE and R hat
summary(jmod)
dct <- dctable(jmod)
plot(dct)
## How to use estimates to make priors more informative?
glmm.model.up <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:p) {
        beta[1,j] ~ dnorm(priors[j,1], priors[j,2])
    }
    sigma ~ dgamma(priors[(p+1),2], priors[(p+1),1])
}
## function for updating, x is an MCMC object
upfun <- function(x) {
    if (missing(x)) {
        p <- ncol(X)
        return(cbind(c(rep(0, p), 0.001), rep(0.001, p+1)))
    } else {
        par <- coef(x)
        return(cbind(par, rep(0.01, length(par))))
    }
}
updat <- list(n = n, Y = Y, X = X, p = ncol(X), priors = upfun())
dcmod <- dc.fit(updat, c("beta", "sigma"), glmm.model.up,
    n.clones = 1:5, multiply = "n", unchanged = "p",
    update = "priors", updatefun = upfun)
summary(dcmod)
dct <- dctable(dcmod)
plot(dct)
plot(dct, type = "sd")
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
mod <- jags.dclone(dat, c("lambda","beta","sigma"), beverton.holt,
    n.clones=c(2, 5, 10), multiply="ncl", unchanged="n")
## compare with results from the paper:
##   beta   = 0.00235
##   lambda = 2.274
##   sigma  = 0.1274
summary(mod)
## FIXME: initsfun example
}
}
\keyword{ models }
\keyword{ htest }
