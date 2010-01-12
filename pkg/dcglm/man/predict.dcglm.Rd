\name{predict.dcglm}
\alias{predict.dcglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.dcglm(object, newdata = NULL, type = c("link", "response"), se = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{newdata}{
%%     ~~Describe \code{newdata} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{se}{
%%     ~~Describe \code{se} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(object, newdata = NULL, type = c("link", "response"), se = FALSE, ...){
    glm.pred <- c("model {",
           "    for (i in 1:n) {",
           "        Y[i] ~ dpois(z[i])",
           "        Y[i] ~ dbin(z[i], k)",
           "        log(z[i]) <- mu[i]",
           "        logit(z[i]) <- mu[i]",
           "        mu[i] <- inprod(X[i,], beta[1,])",
           "    }",
           "    beta[1,1:np] <- mvn[1:np]",
           "    mvn[1:np] ~ dmnorm(coefs[], prec[,])",
           "    }")
    prec <- make.symmetric(solve(vcov(object)))
    coefs <- coef(object)
    if (is.null(newdata)) {
        X <- object$model
    } else {
        rhs <- model.frame(object$formula, newdata)
        X <- model.matrix(attr(rhs, "terms"), rhs)
    }
    type <- match.arg(type)
    params <- switch(type,
        "link" = "mu",
        "response" = "z")
    model <- switch(object$family,
        "poisson" = custommodel(glm.pred, c(4,6)),
        "binomial" = custommodel(glm.pred, c(3,5)))
    prdat <- list(n = nrow(X), X = X, 
        np = ncol(X), k = 1, coefs = coefs, prec = prec)
    prval <- jags.fit(prdat, params, model, ...)
    if (!se) {
        rval <- coef(prval)
    } else {
        rval <- list(fit = coef(prval), 
            se.fit = mcmcapply(prval, sd))
    }
    rval
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
