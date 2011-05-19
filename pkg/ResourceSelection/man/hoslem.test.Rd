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
vector of observations, binary (0/1).
}
  \item{y}{
fitted values.
}
  \item{g}{
number of bins to use.
}
}
\details{
The Hosmer–Lemeshow test is a statistical test for goodness of fit for 
logistic regression models.
}
\value{
An object of class \code{htest}.
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
\dontrun{
ds = read.csv("http://www.math.smith.edu/r/data/help.csv")
m = glm(homeless ~ female + i1 + cesd + age + substance, ds, family=binomial)
hoslem.test(m$y, fitted(m))
}
}
\keyword{ htest }
