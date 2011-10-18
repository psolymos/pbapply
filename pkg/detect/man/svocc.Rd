\name{svocc}
\alias{svocc}
\alias{svocc.fit}
\alias{extractMLE}
\alias{svocc.step}
\title{
ZI Binomial model with single visit
}
\description{
ZI Binomial model with single visit
}
\usage{
svocc(formula, data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, 
method = c("optim", "dc"), inits, 
model = TRUE, x = FALSE, cl = NULL, ...)
svocc.fit(Y, X, Z, link.sta = "cloglog", link.det = "logit", penalized = FALSE, 
auc = FALSE, method = c("optim", "dc"), inits, cl = NULL, ...)
extractMLE(object, ...)
svocc.step(object, model, trace = 1, steps = 1000, 
criter = c("AIC", "BIC", "cAUC"), test = FALSE, k = 2, control, ...)
}
\arguments{
  \item{formula}{
formula of the form \code{y ~ x | z}, where \code{y} is a vector of observations,
\code{x} is the set of covariates for the occurrence model,
\code{z} is the set of covariates for the detection model
}
  \item{Y, X, Z}{
vector of observation, design matrix for occurrence model, and design matrix for detection model
}
  \item{data}{
data
}
  \item{link.sta, link.det}{
link function for the occurrence (true state) and detection model
}
  \item{penalized}{
logical, if penalized likelihood estimate should be computed
}
  \item{method}{
optimization or data cloning to be used as optimization
}
  \item{inits}{
initial values
}
  \item{model}{
a logical value indicating whether model frame should be included as a component of the returned value,
or true state or detection model
}
  \item{x}{
logical values indicating whether the response vector and model matrix used in the 
fitting process should be returned as components of the returned value
}
  \item{cl}{
a cluster object for parallel MCMC calculations if method is data cloning
}
  \item{auc}{
logical, if AUC should be calculated
}
  \item{object}{
a fitted model object
}
  \item{trace}{
info returned during the procedure
}
  \item{steps}{
max number of steps
}
  \item{criter}{
criterion to be minimized (cAUC=1-AUC)
}
  \item{test}{
logical, if decrease in deviance should be tested
}
  \item{k}{
penalty to be used with AIC
}
  \item{control}{
controls for optimization, if missing taken from object
}
  \item{\dots}{
other arguments passed to the functions
}
}
\details{
See Examples.
}
\value{
An object of class 'svocc'.
}
\references{
Lele, S.R., Moreno, M. and Bayne, E. (2011) 
Dealing with detection error in site occupancy surveys: 
What can we do with a single survey? 
\emph{Journal of Plant Ecology} (in press)

Moreno, M. and S.R. Lele (2010) 
Improved estimation of site occupancy using penalized likelihood. 
\emph{Ecology} 91, 341--346.

Lele, S. R., Moreno, M. and Bayne, E. 2011. Dealing with detection error in site
occupancy surveys: What can we do with a single survey? Alberta Biodiversity Monitoring Institute, Alberta,
Canada. Technical Report No. ABMI-20060, August 24, 2011. Available at: 
\href{http://www.abmi.ca/FileDownloadServlet?filename=20060_ABMI_2011-08-24__Single_Visit_Occupancy.pdf&dir=REPORTS_UPLOAD}{http://www.abmi.ca}
}
\author{
Peter Solymos and Monica Moreno
}
\examples{
data(datocc)
## MLE
m00 <- svocc(W ~ x1 | x1 + x3, datocc)
## PMLE
m01 <- svocc(W ~ x1 | x1 + x3, datocc, penalized=TRUE)

## print
m00
## summary
summary(m00)
## coefficients
coef(m00)
## state (occupancy) model estimates
coef(m00, "sta")
## detection model estimates
coef(m00, "det")
## compare estimates
cbind(truth=c(0.6, 0.5, 0.4, -0.5, 0.3), 
mle=coef(m00), pmle=coef(m01))

## AIC, BIC
AIC(m00)
BIC(m00)
## log-likelihood
logLik(m00)
## variance-covariance matrix
vcov(m00)
vcov(m00, model="sta")
vcov(m00, model="det")
## confidence intervals
confint(m00)
confint(m00, model="sta")
confint(m00, model="det")

## fitted values
## (conditional probability of occurrence given detection history:
## if W=1, fitted=1,
## if W=0, fitted=(phi*(1-delta)) / ((1-delta) + phi * (1-delta))
summary(fitted(m00))
## estimated probabilities: (phi*(1-delta)) / ((1-delta) + phi * (1-delta))
summary(m00$estimated.probabilities)
## probability of occurrence (phi)
summary(m00$occurrence.probabilities)
## probability of detection (delta)
summary(m00$detection.probabilities)

\dontrun{
## model selection
m02 <- svocc(W ~ x1 | x3 + x4, datocc)
m03 <- drop1(m02, model="det")
## dropping one term at a time, resulting change in AIC
m03
## updating the model
m04 <- update(m02, . ~ . | . - x4)
m04
## automatic model selection
## part of the model (sta/det) must be specified
m05 <- svocc.step(m02, model="det")
summary(m05)

## nonparametric bootstrap
m06 <- bootstrap(m01, B=25)
attr(m06, "bootstrap")
extractBOOT(m06)
summary(m06, type="mle")
summary(m06, type="pmle") ## no SEs! PMLE!!!
summary(m06, type="boot")
## vcov
#vcov(m06, type="mle") ## this does not work with PMLE
vcov(m06, type="boot") ## this works
## confint
confint(m06, type="boot") ## quantile based

## parametric bootstrap
## sthis is how observations are simulated
head(simulate(m01, 5))
m07 <- bootstrap(m01, B=25, type="param")
extractBOOT(m07)
summary(m07)

data(oven)
ovenc <- oven
ovenc[, c(4:8,10:11)][] <- lapply(ovenc[, c(4:8,10:11)], scale)
ovenc$count01 <- ifelse(ovenc$count > 0, 1, 0)
moven <- svocc(count01 ~ pforest | julian + timeday, ovenc)
summary(moven)
drop1(moven, model="det")
moven2 <- update(moven, . ~ . | . - timeday)
summary(moven)

BIC(moven, moven2)
AUC(moven, moven2)
rocplot(moven)
rocplot(moven2, col=2, add=TRUE)
}
}
\keyword{ htest }
\keyword{ models }
