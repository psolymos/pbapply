\encoding{UTF-8}
\name{singleocc}
\Rdversion{1.1}
\alias{singleocc}
\title{
Zero Inflated Binomial Model for Single Visit Site Occupancy Data
}
\description{
Fit a zero inflated binomial model for single visit site occupancy data
based on maximum likelihood or penalized maximum likelihood estimation.
}
\usage{
singleocc(formula, data, link = "logit", penalized = FALSE, 
method = c("optim", "dc"), n.clones = 1000, 
subset, na.action = na.omit, model = TRUE, x = FALSE, ...)
}
\arguments{
  \item{formula}{
Formula of the form \code{y ~ x | z}, where \code{y} is a vector of detections (1) or nondetections (0),
\code{x} is the set of covariates for the occurrence model,
\code{z} is the set of covariates for the detection model. See Details.
}
  \item{data}{
An optional data frame, list or environment containing the variables in the model. 
If not found in data, the variables are taken from \code{environment(formula)},
typically the environment from which the function is called.
}
  \item{link}{
Character, can be a vector of 2, describing the link function to be used for the
occurrence and the detection parts of the model, respectively. If of length one,
the link is used for both parts. Can be any of \code{c("logit", "probit", "cloglog")}.
}
  \item{penalized}{
Logical, if the penalized maximum likelihood estimate should be calculated. See Details.
}
  \item{method}{
Numerical optimization method to be used. See Details.
}
  \item{n.clones}{
Number of clones, if \code{method = "dc"}.
}
  \item{subset}{
An optional vector specifying a subset of observations to be used in the fitting process.
}
  \item{na.action}{
A function which indicates what should happen when the data contain \code{NA}s.
}
  \item{model}{
A logical value indicating whether model frame should be included as a component of the returned value.
}
  \item{x}{
Logical values indicating whether the response vector and model matrix used in the 
fitting process should be returned as components of the returned value.
}
  \item{\dots}{
Other arguments passed to the functions (e.g. MCMC settings passed to function \code{\link[dclone]{jags.fit}}).
}
}
\details{
The zero inflated binomial (ZIB) model is fitted by maximizing the likelihood function. For small sample
sizes (< 1000) it may overestimate the probability of occurrence. To get less biased estimates,
it is advised to use penalized maximum likelihood estimation (\code{penalized = TRUE}).

If \code{method = "optim"}, the model fitting is done by the quasi-Newton algorithm of \code{\link{optim}}.
Control settings are stored as \code{getOption("occupy.control.optim")}, and can be modified via the
function \code{\link{options}}. Sometimes the optim fit fails. Is such situations, it is recommended to use
\code{method = "dc"} instead.

If \code{method = "dc"}, the model fitting is done by data cloning (Lele et al. 2007).
In this case the fitting is done by the \code{\link[dclone]{jags.fit}} that requires the JAGS software 
(Plummer 2009) installed.
Control settings are stored as \code{getOption("occupy.control.mcmc")}, and can be modified via the
function \code{\link{options}}.

The maximum likelihood estimation can be done by optim an data cloning, but the current implementation of the
penalized maximum likelihood estimation is based on optim (although data cloning can be used to get the
required MLE for the penalization function).

\code{\link{singleocc.fit}} is the workhorse function: 
it is not normally called directly but can be more efficient where the response vector and design mattrices
have already been calculated.
}
\value{
A fitted model of class 'singleocc'.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.

Plummer, M., 2009. JAGS Version 1.0.3 manual (April 23, 2009), \url{http://mcmc-jags.sourceforge.net}
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos} and Monica Moreno
}
\seealso{
Underlying fitting procedure: \code{\link{singleocc.fit}}

Information criteria and other useful tools: \code{\link{AUC}}, \code{\link{BIC}}, \code{\link{rocplot}}, and see Examples.
}
\examples{
## Ovenbird occurrences
data(ovenb)
x <- singleocc(oven ~ uplow | wind+precip+hour+jday+I(hour^2)+I(jday^2), ovenb)
summary(x)
## simulated data
data(rdata)
data(rdata)
m1 <- singleocc(y.obs ~ x1 | x1+x3, rdata)
summary(m1)
m2 <- singleocc(y.obs ~ x1 | x1+x3, rdata, penalized = TRUE)
summary(m2)
## AIC
AIC(m1, m2)
## log-likelihood
logLik(m2)
## coefficients
coef(m2)
## confidence intervals
confint(m2)
## variance covariance matrix
vcov(m2)
## fitted values (probability of occurrence given detection history)
fitted(m2)
## estimated occurrence probabilities
m2$phi
## estimated detection probabilities
m2$delta
}
\keyword{ htest }
\keyword{ models }
