\name{jags.dclone}
\Rdversion{1.1}
\alias{jags.dclone}
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
jags.dclone(data, params, model, n.clones, 
multiply = NULL, unchanged = NULL, 
update = NULL, updatefun = NULL, initsfun=NULL,
trace = 1, ...)
dctable(x, ...)
\method{dctable}{default}(x, ...)
\method{plot}{dctable}(x, which = 1:length(x), 
position="topright", box.cex = 1, ...)
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
A function to use for updating \code{data[[update]]}. See Details and Examples.
}
  \item{initsfun}{
A function to use for generating initial values, \code{inits} are updated by the object
returned by this function from the second iteration. See Examples.
}
  \item{trace}{
  If positive, information is printed during the running of the iterations.
  Higher number indicate more information.
}
  \item{x}{
  An MCMC or a 'dctable' object.
}
  \item{type}{
  What to plot. Besides the default \code{"convergence"} (plot convergence
  diagnostics), character names or integer indices of the estimated parameters
  can be given.
}
  \item{position}{
  Position for the legend, as for \code{\link{legend}}.
}
  \item{box.cex}{
  Scaling factor for the interquartile boxes.
}
  \item{\dots}{
  Other values supplied to \code{\link{jags.fit}}, e.g.
  initial values, \code{n.chains}, \code{n.adapt}, 
    \code{n.update}, \code{thin}, \code{n.iter}.
}
}
\details{
The function fits a JAGS model with increasing numbers of clones, as supplied by
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
## THIS MAY TAKE LONG TIME
## simulation
## Poisson GLMM
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
ncl <- c(1, seq(10, 100, len=10))
n.adapt <- 2000
n.update <- 3000
n.iter <- 5000
## iteartive fitting
jmod <- jags.dclone(jdata, c("beta", "sigma"), jfun1, 
    n.clones = ncl, multiply = "n", unchanged = "np",
    n.adapt = n.adapt, n.update = n.update, n.iter = n.iter)
## summary with DC SE and R hat
## it is also noted if convergence is reached
summary(jmod)
## coralheads example
data(coralheads)
chx <- model.matrix(~log(A), coralheads)
chd <- list(Y = coralheads$S, X = chx, n = nrow(coralheads), np = 2)
chm <- jags.dclone(chd, c("beta", "sigma"), jfun1, 
    n.clones = ncl, multiply = "n", unchanged = "np",
    n.adapt = n.adapt, n.update = n.update, n.iter = n.iter)
dc <- dctable(chm)
opar <- par(mfrow=c(1, 3))
plot(dc, 1)
plot(dc, 2)
plot(dc, 3)
par(opar)
## How to use estimates to make priors more informative?
jfun1v <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(priors[j,1], priors[j,2])
    }
    sigma ~ dlnorm(priors[(np+1),1], priors[(np+1),2])
}
## function for updating, x is an MCMC object
updfun <- function(x) {
    if (missing(x)) {
        par <- coef(glm(S ~ log(A), coralheads, family=poisson))
        return(cbind(c(par, 0), rep(0.001, length(par) + 1)))
    } else {
        par <- coef(x)
        return(cbind(par, rep(0.1, length(par))))
    }
}
chdv <- list(Y = coralheads$S, X = chx, n = nrow(coralheads), np = 2,
    priors = updfun())
chmv <- jags.dclone(chdv, c("beta", "sigma"), jfun1v, 
    n.clones = ncl, multiply = "n", unchanged = "np",
    update = "priors", updatefun = updfun,
    n.adapt = n.adapt, n.update = n.update, n.iter = n.iter)
summary(chmv)
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
