\name{autoupdate}
\Rdversion{1.1}
\alias{autoupdate}
\title{
Automatic updating of an MCMC object
}
\description{
Automatic updating of an MCMC object until a desired statistic value reached.
}
\usage{
autoupdate(object, fun, times, n.update = 0, 
n.iter = niter(object) * thin(object), thin = thin(object), ...)
}
\arguments{
  \item{object}{
A fitted MCMC object ('mcmc.list' class for example), with \code{"updated.model"} attribute.
}
  \item{fun}{
A function that evaluates convergence of the MCMC chains, must return logical result. See Examples.
}
  \item{times}{
Number of times the updating should be repeated. If \code{fun} returns \code{TRUE}, updating
is finished and MCMC object is returned.
}
  \item{n.update}{
Number of updating iterations. The default 0 indicates, that only \code{n.iter} iterations are used.
}
  \item{n.iter}{
Number of iterations for sampling and evaluating \code{fun}.
}
  \item{thin}{
Thinning value.
}
  \item{\dots}{
Other arguments passed to \code{\link[rjags]{coda.samples}}.
}
}
\details{
FIXME Add details here
}
\value{
An MCMC object with \code{"updated.model"} attribute.
}
\references{
REFS???
}
\author{
Peter Solymos
}
\seealso{
\code{\link{}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(object, fun, times, n.update = 0, 
n.iter = niter(object) * thin(object), thin = thin(object), ...)
{
    ## eval of args
    upm <- updated.model(object)
    if (is.null(upm))
        stop("updated model not found")
    mod <- object
    attr(mod, "updated.model") <- NULL    
    fun <- match.fun(fun)
    fval <- fun(mod)
    if (!is.logical(fval))
        stop("'fun' must return logical")
    if (fval)
        return(object)
    ## what to sample
    params <- varnames(mod)
    ## n.update vs. times eval
    if (length(n.update) == 1)
        n.update <- rep(n.update, times)
    ## auto-updating
    for (i in n.update) {
        if (i > 0)
            update(upm, i, ...)
        mod <- coda.samples(upm, params, n.iter, thin, ...)
        if (fun(mod))
            break
    }
    ## put things together
    attr(mod, "updated.model") <- upm
    n.clones <- nclones(object)
    if (!is.null(n.clones) && n.clones > 1) {
        attr(mod, "n.clones") <- n.clones
        class(mod) <- c("mcmc.list.dc", class(mod))
    }
    mod
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
