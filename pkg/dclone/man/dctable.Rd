\name{dc.fit}
\Rdversion{1.1}
\alias{dctable}
\alias{dctable.default}
\alias{extractdctable}
\alias{extractdctable.default}
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
dctable(x, ...)
\method{dctable}{default}(x, ...)
\method{plot}{dctable}(x, which = 1:length(x), 
type = c("all", "var"),
position = "topright", box.cex = 0.75, ...)
extractdctable(x, ...)
\method{extractdctable}{default}(x, ...)
}
\arguments{
  \item{x}{
  An MCMC or a 'dctable' object.
}
  \item{\dots}{
  Optionally more fitted model objects for function \code{dctable}.
}
  \item{which}{
  What to plot, character names or integer indices of the estimated parameters
  are accepted.
}
  \item{type}{
  Type of plot to be drawn. \code{all} results in plotting means, standard deviations and quantiles
  agains the number of clones as boxplot. \code{var} reults in plotting the scaled variances
  agains the number of clones. In this case variances are divided by the variance of the
  model with smallest number of clones, \code{min(n.clones)}.
  Along with the values, the \code{min(n.clones) / n.clones} line is plotted for reference.
}
  \item{position}{
  Position for the legend, as for \code{\link{legend}}.
}
  \item{box.cex}{
  Scaling factor for the interquartile boxes.
}
}
\details{
\code{dctable} returns the \code{"dctable"} attribute of the MCMC object,
or if it is \code{NULL}, calculates the \code{dctable} summaries.
If more than one fitted objects are provided, summaries are calculated
for all objects.
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
plot(dct, type = "var")
}
}
\keyword{ models }
\keyword{ htest }
