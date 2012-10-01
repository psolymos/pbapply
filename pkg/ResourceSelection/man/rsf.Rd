\name{rsf}
\alias{rsf.fit}
\alias{rsf}
\alias{rspf}
\title{
Resource Selection (Probability) Functions for Use-Availability Data
}
\description{
  Resource Selection (Probability) Functions for 
  use-availability wildlife data as described in 
  Lele and Keim (2006) and Lele (2009).
}
\usage{
rsf(formula, data, m, B = 99, inits, method = "Nelder-Mead", 
control, model = TRUE, x = FALSE, ...)

rspf(formula, data, m, B = 99, link = "logit", inits, 
method = "Nelder-Mead", control, model = TRUE, x = FALSE, ...)

rsf.fit(X, Y, m, link = "logit", B = 99, 
inits, method = "Nelder-Mead", control, ...)
}
\arguments{
  \item{formula}{
two sided model formula of the form \code{y ~ x}, 
where \code{y} is a vector of observations,
\code{x} is the set of covariates.
}
  \item{m}{
argument describing the matching of use and available points.
All available points are used for each use points if \code{m=0}
(global availability). If \code{m} is a single value, e.g. \code{m=5},
it is assumed that available data points are grouped in batches of 5,
e.g. with IDs \code{c(1,2)} for used point locations and 
\code{c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)} for available locations
(local availability, matched use-available design).
Similarly, a vector of matching IDs can also be provided, e.g.
\code{c(1, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2)} by combining the above two.
This potentially could allow for unbalanced matching and for easier 
subsetting of the data, but comes with an increased computing time.
}
  \item{data}{
data.
}
  \item{B}{
number of bootstrap iterations to make.
}
  \item{link}{
character, lype of link function to be used.
}
  \item{inits}{
initial values, optional.
}
  \item{method}{
method to be used in \code{\link{optim}} for numerical optimization.
}
  \item{control}{
control options for \code{\link{optim}}.
}
  \item{model}{
a logical value indicating whether model frame should be included as a component of the returned value
}
  \item{x}{
logical values indicating whether the model matrix used in the 
fitting process should be returned as components of the returned value.
}
\item{Y}{
vector of observations.
}
\item{X}{
covariate matrix.
}
  \item{\dots}{
other arguments passed to the functions.
}
}
\details{
The \code{rsf} function fits the Exponential Resource Selection Function
(RSF) model to presence only data.

The \code{rspf} function fits the Resource Selection Probability Function
(RSPF) model to presence only data Link function \code{"logit"}, 
\code{"cloglog"}, and \code{"probit"} can be specified via the 
\code{link} argument.

The \code{rsf.fit} is the workhorse behind the two functions.
\code{link="log"} leads to Exponential RSF.

LHS of the \code{formula} data must be binary, ones indicating used locations,
while zeros indicating available location.

For model description and estimation details, see Lele and Keim (2006)
and Lele (2009).
}
\value{
A list with class \code{"rsf"} or \code{"rspf"}
containing the following components: 
  \item{call}{the matched call.}
  \item{y}{vector from LHS of the formula.}
  \item{coefficients}{a named vector of coefficients.}
  \item{std.error}{a named vector of standard errors
    for the coefficients}
  \item{loglik}{the maximized log-likelihood}
  \item{results}{\code{\link{optim}} results.}
  \item{link}{character, value of the link function used.}
  \item{control}{control parameters for \code{\link{optim}}.}
  \item{inits}{initial values used in optimization.}
  \item{m}{component for future developmen, currently it takes value 0.}
  \item{np}{number of active parameters.}
  \item{fitted.values}{vector of fitted values. These are relative
    selection values for RSF models, and probability of selection for 
    RSPF models.}
  \item{nobs}{number of used locations.}
  \item{bootstrap}{component to store bootstrap results if \code{B}>0.}
  \item{converged}{logical, indicating convergence of the optimization.}
  \item{formula}{the formula supplied.}
  \item{terms}{the \code{\link{terms}} object used.}
  \item{levels}{a record of the levels of the factors used in fitting.}
  \item{contrasts}{the contrasts used.}
  \item{model}{if requested, the model frame.}
  \item{x}{if requested, the model matrix.}
}
\references{
Lele, S.R. (2009) 
A new method for estimation of resource selection probability function. 
Journal of Wildlife Management 73, 122--127.

Lele, S. R. &  Keim, J. L. (2006) 
Weighted distributions and estimation of resource selection probability functions. 
Ecology 87, 3021--3028.
}
\author{
Subhash R. Lele, Jonah L. Keim, Peter Solymos
}
\examples{
## settings
n.used <- 1000
m <- 10
n <- n.used * m
set.seed(1234)
x <- data.frame(x1=rnorm(n), x2=runif(n))
cfs <- c(1.5,-1,0.5)
## fitting Exponential RSF model
dat1 <- simulateUsedAvail(x, cfs, n.used, m, link="log")
m1 <- rsf(status ~ .-status, dat1, m=0, B=0)
summary(m1)
## fitting Logistic RSPF model
dat2 <- simulateUsedAvail(x, cfs, n.used, m, link="logit")
m2 <- rspf(status ~ .-status, dat2, m=0, B=0)
summary(m2)
}
\keyword{ htest }
