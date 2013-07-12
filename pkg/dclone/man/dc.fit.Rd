\encoding{UTF-8}
\name{dc.fit}
\Rdversion{1.1}
\alias{dc.fit}
\title{
Iterative model fitting with data cloning
}
\description{
\code{\link{jags.fit}} or \code{\link{bugs.fit}} is 
iteratively used to fit a model with
increasing the number of clones.
}
\usage{
dc.fit(data, params, model, inits, n.clones, 
    multiply = NULL, unchanged = NULL, 
    update = NULL, updatefun = NULL, initsfun = NULL,
    flavour = c("jags", "bugs"), n.chains = 3, ...)
}
\arguments{
  \item{data}{
A named list (or environment) containing the data.
}
  \item{params}{
Character vector of parameters to be sampled.
It can be a list of 2 vectors, 1st element
is used as parameters to monitor, the 2nd is used
as parameters to use in calculating the data cloning
diagnostics.
}
  \item{model}{
Character string (name of the model file), a function containing 
the model, or a \code{\link{custommodel}} object (see Examples).
}
  \item{inits}{
Optional specification of initial values in the form of a list or a 
function (see Initialization at \code{\link[rjags]{jags.model}}). 
If missing, will be treated as \code{NULL} and initial values will 
be generated automatically.
}
  \item{n.clones}{
An integer vector containing the numbers of clones to use itaratively.
}
  \item{multiply}{
Numeric or character index for list element(s) in the \code{data} 
argument to be multiplied by the number of clones instead of repetitions.
}
  \item{unchanged}{
Numeric or character index for list element(s) in the \code{data} 
argument to be left unchanged.
}
  \item{update}{
Numeric or character index for list element(s) in the \code{data} 
argument that has to be updated by \code{updatefun} in each iterations. 
This usually is for making priors more informative, and enhancing 
convergence. See Details and Examples.
}
  \item{updatefun}{
A function to use for updating \code{data[[update]]}. 
It should take an 'mcmc.list' object as 1st argument,
2nd argument can be the number of clones.
See Details and Examples.
}
  \item{initsfun}{
A function to use for generating initial values, \code{inits} are 
updated by the object
returned by this function from the second iteration. If initial values
are not dependent on the previous iteration, this should be \code{NULL}, 
otherwise, it should take an 'mcmc.list' object as 1st argument, 
2nd argument can be the number of clones. 
This feature is useful if latent nodes are provided in \code{inits}
so it also requires to be cloned for subsequent iterations.
See Details and Examples.
}
  \item{flavour}{
  If \code{"jags"}, the function \code{\link{jags.fit}} is called. 
  If \code{"bugs"}, the function \code{\link{bugs.fit}} is called.
}
  \item{n.chains}{
Number of chains to generate.
}
  \item{\dots}{
  Other values supplied to \code{\link{jags.fit}}, or 
  \code{\link{bugs.fit}}, depending on the \code{flavour} argument.
}
}
\details{
The function fits a JAGS/BUGS model with increasing numbers of clones, 
as supplied by the argument \code{n.clones}. Data cloning is done by the 
function \code{dclone} using
the arguments \code{multiply} and \code{unchanged}.
An updating function can be provided, see Examples.
}
\value{
An object inheriting from the class 'mcmc.list'.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.

Lele, S. R., K. Nadeem and B. Schmuland, 2010.
Estimability and likelihood inference for generalized linear
mixed models using data cloning.
\emph{Journal of the American Statistical Association}
\strong{105}, 1617--1625.

Solymos, P., 2010. dclone: Data Cloning in R.
\emph{The R Journal} \strong{2(2)}, 29--37.
URL: \url{http://journal.r-project.org/archive/2010-2/RJournal_2010-2_Solymos.pdf}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}, 
implementation is based on many discussions 
with Khurram Nadeem and Subhash Lele.
}
\seealso{
Data cloning: \code{\link{dclone}}.

Parallel computations: \code{\link{dc.parfit}}

Model fitting: \code{\link{jags.fit}}, \code{\link{bugs.fit}}

Convergence diagnostics: \code{\link{dctable}}, \code{\link{dcdiag}}
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
linpred <- X \%*\% beta + alpha
Y <- rpois(n, exp(linpred))
## JAGS model as a function
jfun1 <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta)
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:np) {
        beta[j] ~ dnorm(0, 0.001)
    }
    sigma ~ dlnorm(0, 0.001)
}
## data
jdata <- list(n = n, Y = Y, X = X, np = NCOL(X))
## inits with latent variable and parameters
ini <- list(alpha=rep(0,n), beta=rep(0, NCOL(X)))
## function to update inits
ifun <- function(model, n.clones) {
    list(alpha=dclone(rep(0,n), n.clones),
        beta=coef(model)[-length(coef(model))])
}
## iteartive fitting
jmod <- dc.fit(jdata, c("beta", "sigma"), jfun1, ini,
    n.clones = 1:5, multiply = "n", unchanged = "np",
    initsfun=ifun)
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
## time series example
## data and model taken from Ponciano et al. 2009
## Ecology 90, 356-362.
paurelia <- c(17,29,39,63,185,258,267,392,510,
    570,650,560,575,650,550,480,520,500)
dat <- list(ncl=1, n=length(paurelia), Y=dcdim(data.matrix(paurelia)))
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
    n.clones=c(1, 2, 5, 10), multiply="ncl", unchanged="n")
## compare with results from the paper:
##   beta   = 0.00235
##   lambda = 2.274
##   sigma  = 0.1274
summary(mod)

## Using WinBUGS/OpenBUGS
if (.Platform$OS.type == "windows") {
data(schools)
dat <- list(J = nrow(schools), y = schools$estimate, 
    sigma.y = schools$sd)
bugs.model <- function(){
       for (j in 1:J){
         y[j] ~ dnorm (theta[j], tau.y[j])
         theta[j] ~ dnorm (mu.theta, tau.theta)
         tau.y[j] <- pow(sigma.y[j], -2)
       }
       mu.theta ~ dnorm (0.0, 1.0E-6)
       tau.theta <- pow(sigma.theta, -2)
       sigma.theta ~ dunif (0, 1000)
     }  
inits <- function(){
    list(theta=rnorm(nrow(schools), 0, 100), mu.theta=rnorm(1, 0, 100),
         sigma.theta=runif(1, 0, 100))
}
param <- c("mu.theta", "sigma.theta")
sim2 <- try(dc.fit(dat, param, bugs.model, n.clones=1:2, 
    flavour="bugs", program="WinBUGS", multiply="J"))
sim3 <- try(dc.fit(dat, param, bugs.model, n.clones=1:2, 
    flavour="bugs", program="OpenBUGS", multiply="J"))
}
}
}
\keyword{ models }
\keyword{ htest }
