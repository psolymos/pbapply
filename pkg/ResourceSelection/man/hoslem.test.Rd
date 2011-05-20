\name{hoslem.test}
\alias{hoslem.test}
\title{
Hosmer-Lemeshow Goodness of Fit (GOF) Test
}
\description{
Hosmer-Lemeshow Goodness of Fit (GOF) Test.
}
\usage{
hoslem.test(x, y, g = 10)
}
\arguments{
  \item{x}{
a numeric vector of observations, binary (0/1).
}
  \item{y}{
expected values.
}
  \item{g}{
number of bins to use to calculate quantiles.
}
}
\details{
The Hosmer-Lemeshow test is a statistical test for goodness of fit for 
logistic regression models.
}
\value{
A list with class \code{"htest"} containing the following components: 
  \item{statistic}{the value of the chi-squared test statistic,
    (\code{sum((observed - expected)^2 / expected)}).}
  \item{parameter}{the degrees of freedom of the approximate 
    chi-squared distribution of the test statistic (\code{g - 2}).}
  \item{p.value}{the p-value for the test.}
  \item{method}{a character string indicating the type of test performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{the observed frequencies in a \code{g}-by-2 
    contingency table.}
  \item{expected}{the expected frequencies in a \code{g}-by-2 
    contingency table.}
}
\references{
Hosmer D W, Lemeshow S 2000. 
Applied Logistic Regression. 
New York, USA: John Wiley and Sons.
}
\author{
Peter Solymos by adapting code pieces from R help mailing list
}
\examples{
set.seed(123)
n <- 500
x <- rnorm(n)
y <- rbinom(n, 1, plogis(0.1 + 0.5*x))
m <- glm(y ~ x, family=binomial)
hoslem.test(m$y, fitted(m))
}
\keyword{ htest }
