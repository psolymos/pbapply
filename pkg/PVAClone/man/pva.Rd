\name{pva}
\alias{pva}
\title{
Population Viability Analysis
}
\description{
Populatio Viability Analysis (PVA).
}
\usage{
pva(x, model, n.clones, ...)
}
\arguments{
  \item{x}{
Numeric, a time series. Values must be non-negative,
missing values are allowed (but first and last observation
must not be missing).
}
  \item{model}{
A 'pvamodel' object returned by a function, see Examples.
}
  \item{n.clones}{
Numeric, number of clones (possibly a vector).
}
  \item{\dots}{
Arguments passed to underlying fitting functions,
most notably \code{n.update}, \code{n.iter},
\code{n.chains}, \code{thin}, \code{cl}.
See \code{\link[dcmle]{dcmle}}.
}
}
\details{

The function implements the first step in PVA, i.e. to fit a 
given growth model to a population time series 
data (Nadeem and Lele, 2012). The function employs 
Lele et. al's (2007, 2010) data cloning (DC) algorithm for computing 
the maximum likelihood estimates of model parameters along with the 
corresponding standrad errors. See Solymos (2010) for 
an R implementaion of the DC algorithm. The growth models currently 
available in the package PVAClone are 
Gompertz (\code{\link{gompertz}}), Ricker (\code{\link{ricker}}), 
logistic, theta-logistic, Beverton-Holt and the generalized Beverton-Holt. 

These models can also be fitted assuming 
the presence of observation error using the general state-space model 
formulation (Nadeem and Lele, 2012). Currently the Normal and Poisson
observation error models are supported.

Normal observation error model: 
\eqn{y_{t} \sim Normal(x_{t}, \tau^2)}{%
y[t] \~ Normal(x[t], tau^2)}, 
where \eqn{y_{t}}{y[t]} is the estimated abundance 
on the log scale at time \eqn{t}.

Poisson observation error model: 
\eqn{O_{t} \sim Poisson(e^{x_{t}})}{%
O[t] \~ Poisson(e^x[t])}, 
where \eqn{O_{t}}{O[t]} is the estimated abundance at time \eqn{t}.

In addition, missing observations can be accommodated in both with or 
without observation error cases.

}
\value{
An object of class 'pva', see \code{\link{pva-class}}.
}
\references{

Lele, S.R., B. Dennis and F. Lutscher, 2007. 
Data cloning: easy maximum likelihood estimation for
complex ecological models using Bayesian Markov 
chain Monte Carlo methods. Ecology Letters 10, 551-563.

Lele, S. R., K. Nadeem and B. Schmuland, 2010. 
Estimability and likelihood inference for generalized
linear mixed models using data cloning. 
Journal of the American Statistical Association 105,
1617-1625.

Nadeem, K., Lele S. R., 2012.  
Likelihood based population viability analysis in the presence of 
observation error. Oikos. doi: 10.1111/j.1600-0706.2011.20010.x

Solymos, P., 2010. dclone: Data Cloning in R. 
The R Journal 2(2), 29-37. URL: 
\url{http://journal.r-project.org/archive/2010-2/RJournal_2010-2_Solymos.pdf}

}
\author{
Khurram Nadeem and Peter Solymos
}
\seealso{
\code{\link{model.select}}

\code{\link{pva-class}}, \code{\link{pvamodel-class}}
}
\examples{
\dontrun{
data(redstart)
data(paurelia)

## Gompertz
m1 <- pva(redstart, "gompertz", c(5,10))
m2 <- pva(redstart, gompertz("poisson"), c(5,10))
m3 <- pva(redstart, gompertz("normal"), c(5,10))
m1na <- pva(paurelia, "gompertz", c(5,10))
m2na <- pva(paurelia, gompertz("poisson"), c(5,10))
m3na <- pva(paurelia, gompertz("normal"), c(5,10))
m1x <- pva(redstart, gompertz("normal"), 5)
m2x <- pva(redstart, gompertz("normal", fixed=c(tau=0.1)), 5)

## Ricker
m1 <- pva(redstart, "ricker", c(5,10))
m2 <- pva(redstart, ricker("poisson"), c(5,10))
m3 <- pva(redstart, ricker("normal"), c(5,10))
m1na <- pva(paurelia, "ricker", c(5,10))
m2na <- pva(paurelia, ricker("poisson"), c(5,10))
m3na <- pva(paurelia, ricker("normal"), c(5,10))
m1x <- pva(redstart, ricker("normal"), 5)
m2x <- pva(redstart, ricker("normal", fixed=c(tau=0.1)), 5)

m2x
summary(m2x)
coef(m2x)
vcov(m2x)
confint(m2x)
plot(m2x)
plot(as.mcmc.list(m2x, diagn_scale=TRUE))
}
}
\keyword{ htest }
\keyword{ models }
\keyword{ ts }
