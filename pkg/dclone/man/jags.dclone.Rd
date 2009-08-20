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
trace = 1, stop.if.converged = TRUE, ...)
dctable(x, ...)
\method{dctable}{default}(x, ...)
\method{plot}{dctable}(x, type = "convergence", 
position="topleft", box.cex = 1, ...)
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
  \item{stop.if.converged}{
  Logical, if \code{FALSE} the iteration goes through the values in \code{n.clones}
  even if convergence has been reached. If \code{TRUE} it returns the model 
  once converged, and ignores the rest in \code{n.clones}.
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

After each model fit, data cloning convergence is tested based on the maximal
eigenvalue of the posterior unscaled variance-covariance matrix
(\code{\link{lambdamax.diag}}) and the p-value of the Shapiro-Wilk multivariate 
normality test (\code{\link{shapiro.diag}}).

Critical values for these two diagnostics are arbitrarily set by 0.05
(\code{getOption("dclone.crit")}). Values can be changed to e.g. to
as \code{options("dclone.crit" = c(0.01,0.1))} for stricter criteria.

\code{dctable} returns the \code{"dctable"} attribute of an MCMC object.
This attribute is created during the iterative data cloning process,
to store convergence diagnostics and statistics for the sampled parameters,
because only the last MCMC object is kept. This way, there is a track of
changes in the estimates.

Identifyability issues can also be tracked by statistics in the \code{dctable}
attribute, see Examples how to retrieve these information, and for plotting.

Fixme !!! Add UPDATE details !!! 
}
\value{
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
\note{
}
\seealso{
Data cloning: \code{\link{dclone}}

Model fitting: \code{\link{jags.fit}}

Diagnostics: \code{\link{lambdamax.diag}}, \code{\link{shapiro.diag}}, 
for critical values see \code{getOption("dclone.crit")}.
}
\examples{
\dontrun{
## THIS MAY TAKE LONG TIME
## simulation
## Poisson GLMM
set.seed(1234)
n <- 100
beta <- c(1, -1)
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
        log(lambda[i]) <- alpha[i] + inprod(X[i,], betas[i,])
        for (j in 1:np) {
            betas[i,j] <- beta[j]
        }
        alpha[i] ~ dnorm(0, tau)
    }
    for (j in 1:np) {
        beta[j] ~ dnorm(0, 0.001)
    }
    tau ~ dgamma(1.0E-3, 1.0E-3)
    sigma <- 1.0/sqrt(tau)
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
opar <- par(mfrow=c(2, 2))
plot(dc)
plot(dc, 1)
plot(dc, 2)
plot(dc, 3)
par(opar)
## Fixme! add UPDATE example
## Binomial GLMM and non-identifyability
jfun2 <- function() {
    for (i in 1:n) {
        Y[i] ~ dbin(p[i], k)
        logit(p[i]) <- alpha[i] + inprod(X[i,], betas[i,])
        for (j in 1:np) {
            betas[i,j] <- beta[j]
        }
        alpha[i] ~ dnorm(0, tau)
    }
    for (j in 1:np) {
        beta[j] ~ dnorm(0, 0.001)
    }
    tau ~ dgamma(1.0E-3, 1.0E-3)
    sigma <- 1.0/sqrt(tau)
}
Y2 <- rbinom(n, 1, exp(linpred) / (1 + exp(linpred)))
jdata2 <- list(n = n, Y = Y2, X = X, np = NCOL(X), k = 1)
## iteartive fitting
## note: Binomial model requires only multiplication
## and not repetition for DC
jmod2 <- jags.dclone(jdata2, c("beta", "sigma"), jfun2, 
    n.clones = ncl, multiply = c("Y", "k"), unchanged = c("n", "X", "np"),
    n.adapt = n.adapt, n.update = n.update, n.iter = n.iter)
## inspect unscaled SD values for beta[1] and sigma
## if SD is not decreasing with the number of clones
## then parameters are likely non-identifyable
dc2 <- dctable(jmod2)
dc2$convergence
lapply(dc2$statistics, function(z) z$sd)
## How to use estimates to make priors more informative?
jfun1v <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], betas[i,])
        for (j in 1:np) {
            betas[i,j] <- beta[j]
        }
        alpha[i] ~ dnorm(0, tau)
    }
    for (j in 1:np) {
        beta[j] ~ dnorm(priors[j,1], priors[j,2])
    }
    tau ~ dgamma(priors[(np+1),2], priors[(np+1),1])
    sigma <- 1.0/sqrt(tau)
}
## function for updating, x is an MCMC object
updfun <- function(x) {
    if (missing(x)) {
        par <- coef(glm(S ~ log(A), coralheads, family=poisson))
        return(cbind(c(par, 0.001), rep(0.01, length(par) + 1)))
    } else {
        par <- report(x, mean)
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
## FIXME: initsfun example
}
}
\keyword{ models }
\keyword{ htest }
