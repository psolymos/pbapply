\name{dcglm}
\alias{dcglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dcglm(formula, data = parent.frame(), family = c("poisson", "binomial"), n.clones = 5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
}
  \item{n.clones}{
%%     ~~Describe \code{n.clones} here~~
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
function(formula, data = parent.frame(), family=c("poisson", "binomial"), n.clones=5, ...){
    glm.model <- c("model {",
                   "    for (i in 1:n) {",
                   "        Y[i] ~ dpois(lambda[i])",
                   "        Y[i] ~ dbin(p[i], k)",
                   "        log(lambda[i]) <- inprod(X[i,], beta[1,])",
                   "        logit(p[i]) <- inprod(X[i,], beta[1,])",
                   "    }",
                   "    for (j in 1:np) {",
                   "        beta[1,j] ~ dnorm(0, 0.001)",
                   "    }",
                   "}")
    family <- match.arg(family)
    lhs <- formula[[2]]
    Y <- eval(lhs, data)
    formula[[2]] <- NULL
    rhs <- model.frame(formula, data)
    X <- model.matrix(attr(rhs, "terms"), rhs)
    dat <- list(n = length(Y), Y = Y, X = X, np = ncol(X), k = 1)
    if (family == "poisson") {
        model <- model <- custommodel(glm.model, c(4,6))
        dcdat <- dclone(dat, n.clones, multiply = "n", unchanged = "np")
    } else {
        model <- custommodel(glm.model, c(3,5))
        dcdat <- dclone(dat, n.clones, multiply = c("Y","k"), unchanged = c("n", "np", "X"))
    }
    mod <- jags.fit(dcdat, "beta", model, ...)
    COEF <- coef(mod)
    SE <- dcsd(mod)
    names(COEF) <- names(SE) <- colnames(X)
    mu <- X \%*\% COEF
    if (family == "poisson") {
        fitval <- exp(mu)
        ll <- sum(log(fitval^Y * exp(-fitval)) - log(factorial(Y)))
    } else {
        fitval <- exp(mu) / (1 + exp(mu))
        ll <- sum(log(choose(1, Y) * fitval^Y * (1-fitval)^(1-Y)))
    }
    rval <- list(call=match.call(),
        mcmc = mod,
        y = Y,
        x = rhs, 
        model = X,
        fitted.values = fitval,
        linear.predictors = mu,
        formula = formula,
        coefficients = COEF,
        std.error = SE,
        loglik = ll,
        family = family,
        df.residual = length(Y) - length(COEF),
        df.null = length(Y) - 1)
    class(rval) <- c("dcglm")
    rval
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
