\name{codaMCMC-class}
\Rdversion{1.1}
\docType{class}
\alias{codaMCMC-class}
\alias{[,codaMCMC-method}
\alias{[[,codaMCMC-method}
\alias{acfplot,codaMCMC-method}
\alias{as.array,codaMCMC-method}
\alias{as.matrix,codaMCMC-method}
\alias{as.mcmc.list,codaMCMC-method}
\alias{autocorr.diag,codaMCMC-method}
\alias{chanames,codaMCMC-method}
\alias{chisq.diag,codaMCMC-method}
\alias{coef,codaMCMC-method}
\alias{coerce,codaMCMC,dcmle-method}
\alias{coerce,codaMCMC,MCMClist-method}
\alias{coerce,MCMClist,codaMCMC-method}
\alias{confint,codaMCMC-method}
\alias{crosscorr.plot,codaMCMC-method}
\alias{crosscorr,codaMCMC-method}
\alias{dcdiag,codaMCMC-method}
\alias{dcsd,codaMCMC-method}
\alias{dctable,codaMCMC-method}
\alias{densityplot,codaMCMC-method}
\alias{densplot,codaMCMC-method}
\alias{end,codaMCMC-method}
\alias{frequency,codaMCMC-method}
\alias{gelman.diag,codaMCMC-method}
\alias{gelman.plot,codaMCMC-method}
\alias{geweke.diag,codaMCMC-method}
\alias{head,codaMCMC-method}
\alias{heidel.diag,codaMCMC-method}
\alias{lambdamax.diag,codaMCMC-method}
\alias{mcpar,codaMCMC-method}
\alias{nchain,codaMCMC-method}
\alias{nclones,codaMCMC-method}
\alias{niter,codaMCMC-method}
\alias{nvar,codaMCMC-method}
\alias{pairs,codaMCMC-method}
\alias{plot,codaMCMC,missing-method}
\alias{qqmath,codaMCMC-method}
\alias{quantile,codaMCMC-method}
\alias{raftery.diag,codaMCMC-method}
\alias{show,codaMCMC-method}
\alias{stack,codaMCMC-method}
\alias{start,codaMCMC-method}
\alias{summary,codaMCMC-method}
\alias{tail,codaMCMC-method}
\alias{thin,codaMCMC-method}
\alias{time,codaMCMC-method}
\alias{traceplot,codaMCMC-method}
\alias{varnames,codaMCMC-method}
\alias{vcov,codaMCMC-method}
\alias{window,codaMCMC-method}
\alias{xyplot,codaMCMC-method}

\title{Class \code{"codaMCMC"}}
\description{
An S4 representation of an mcmc.lits object of the \pkg{coda} package.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("codaMCMC", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{values}:}{Object of class \code{"numeric"},
        values from the posterior sample of length
        \code{niter * nvar * nchains}.}
    \item{\code{varnames}:}{Object of class \code{"character"},
        variable names.}
    \item{\code{start}:}{Object of class \code{"integer"},
        start of iterations.}
    \item{\code{end}:}{Object of class \code{"integer"},
        end of iterations.}
    \item{\code{thin}:}{Object of class \code{"integer"},
        thinning value.}
    \item{\code{nchains}:}{Object of class \code{"integer"},
        number of chains.}
    \item{\code{niter}:}{Object of class \code{"integer"},
        number of iterations.}
    \item{\code{nvar}:}{Object of class \code{"integer"},
        number of variables}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "codaMCMC")}: ... }
    \item{[[}{\code{signature(x = "codaMCMC")}: ... }
    \item{acfplot}{\code{signature(x = "codaMCMC")}: ... }
    \item{as.array}{\code{signature(x = "codaMCMC")}: ... }
    \item{as.matrix}{\code{signature(x = "codaMCMC")}: ... }
    \item{as.mcmc.list}{\code{signature(x = "codaMCMC")}: ... }
    \item{autocorr.diag}{\code{signature(mcmc.obj = "codaMCMC")}: ... }
    \item{chanames}{\code{signature(x = "codaMCMC")}: ... }
    \item{chisq.diag}{\code{signature(x = "codaMCMC")}: ... }
    \item{coef}{\code{signature(object = "codaMCMC")}: ... }
    \item{coerce}{\code{signature(from = "codaMCMC", to = "dcmle")}: ... }
    \item{coerce}{\code{signature(from = "codaMCMC", to = "MCMClist")}: ... }
    \item{coerce}{\code{signature(from = "dcmle", to = "codaMCMC")}: ... }
    \item{coerce}{\code{signature(from = "MCMClist", to = "codaMCMC")}: ... }
    \item{confint}{\code{signature(object = "codaMCMC")}: ... }
    \item{crosscorr.plot}{\code{signature(x = "codaMCMC")}: ... }
    \item{crosscorr}{\code{signature(x = "codaMCMC")}: ... }
    \item{dcdiag}{\code{signature(x = "codaMCMC")}: ... }
    \item{dcsd}{\code{signature(object = "codaMCMC")}: ... }
    \item{dctable}{\code{signature(x = "codaMCMC")}: ... }
    \item{densityplot}{\code{signature(x = "codaMCMC")}: ... }
    \item{densplot}{\code{signature(x = "codaMCMC")}: ... }
    \item{end}{\code{signature(x = "codaMCMC")}: ... }
    \item{frequency}{\code{signature(x = "codaMCMC")}: ... }
    \item{gelman.diag}{\code{signature(x = "codaMCMC")}: ... }
    \item{gelman.plot}{\code{signature(x = "codaMCMC")}: ... }
    \item{geweke.diag}{\code{signature(x = "codaMCMC")}: ... }
    \item{head}{\code{signature(x = "codaMCMC")}: ... }
    \item{heidel.diag}{\code{signature(x = "codaMCMC")}: ... }
    \item{lambdamax.diag}{\code{signature(x = "codaMCMC")}: ... }
    \item{mcpar}{\code{signature(x = "codaMCMC")}: ... }
    \item{nchain}{\code{signature(x = "codaMCMC")}: ... }
    \item{nclones}{\code{signature(x = "codaMCMC")}: ... }
    \item{niter}{\code{signature(x = "codaMCMC")}: ... }
    \item{nvar}{\code{signature(x = "codaMCMC")}: ... }
    \item{pairs}{\code{signature(x = "codaMCMC")}: ... }
    \item{plot}{\code{signature(x = "codaMCMC", y = "missing")}: ... }
    \item{qqmath}{\code{signature(x = "codaMCMC")}: ... }
    \item{quantile}{\code{signature(x = "codaMCMC")}: ... }
    \item{raftery.diag}{\code{signature(x = "codaMCMC")}: ... }
    \item{show}{\code{signature(object = "codaMCMC")}: ... }
    \item{stack}{\code{signature(x = "codaMCMC")}: ... }
    \item{start}{\code{signature(x = "codaMCMC")}: ... }
    \item{summary}{\code{signature(object = "codaMCMC")}: ... }
    \item{tail}{\code{signature(x = "codaMCMC")}: ... }
    \item{thin}{\code{signature(x = "codaMCMC")}: ... }
    \item{time}{\code{signature(x = "codaMCMC")}: ... }
    \item{traceplot}{\code{signature(x = "codaMCMC")}: ... }
    \item{varnames}{\code{signature(x = "codaMCMC")}: ... }
    \item{vcov}{\code{signature(object = "codaMCMC")}: ... }
    \item{window}{\code{signature(x = "codaMCMC")}: ... }
    \item{xyplot}{\code{signature(x = "codaMCMC")}: ... }
	 }
}
\author{
Peter Solymos
}
\seealso{
\code{\link[coda]{mcmc.list}}
}
\examples{
showClass("codaMCMC")
}
\keyword{classes}
