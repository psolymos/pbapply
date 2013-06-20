\name{MCMClist-class}
\Rdversion{1.1}
\docType{class}
\alias{MCMClist-class}
\alias{acfplot,MCMClist-method}
\alias{autocorr.diag,MCMClist-method}
\alias{chanames,MCMClist-method}
\alias{chisq.diag,MCMClist-method}
\alias{confint,MCMClist-method}
\alias{crosscorr.plot,MCMClist-method}
\alias{crosscorr,MCMClist-method}
\alias{densityplot,MCMClist-method}
\alias{densplot,MCMClist-method}
\alias{frequency,MCMClist-method}
\alias{gelman.diag,MCMClist-method}
\alias{gelman.plot,MCMClist-method}
\alias{geweke.diag,MCMClist-method}
\alias{heidel.diag,MCMClist-method}
\alias{lambdamax.diag,MCMClist-method}
\alias{mcpar,MCMClist-method}
\alias{nchain,MCMClist-method}
\alias{niter,MCMClist-method}
\alias{nvar,MCMClist-method}
\alias{pairs,MCMClist-method}
\alias{plot,MCMClist,missing-method}
\alias{qqmath,MCMClist-method}
\alias{quantile,MCMClist-method}
\alias{raftery.diag,MCMClist-method}
\alias{thin,MCMClist-method}
\alias{traceplot,MCMClist-method}
\alias{varnames,MCMClist-method}
\alias{xyplot,MCMClist-method}

\title{Class \code{"MCMClist"}}
\description{
Virtual class for S3 mcmc.list object from \pkg{coda} package.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
  \describe{
    \item{acfplot}{\code{signature(x = "MCMClist")}: ... }
    \item{autocorr.diag}{\code{signature(mcmc.obj = "MCMClist")}: ... }
    \item{chanames}{\code{signature(x = "MCMClist")}: ... }
    \item{chisq.diag}{\code{signature(x = "MCMClist")}: ... }
    \item{coerce}{\code{signature(from = "codaMCMC", to = "MCMClist")}: ... }
    \item{coerce}{\code{signature(from = "dcCodaMCMC", to = "MCMClist")}: ... }
    \item{coerce}{\code{signature(from = "dcmle", to = "MCMClist")}: ... }
    \item{coerce}{\code{signature(from = "MCMClist", to = "codaMCMC")}: ... }
    \item{coerce}{\code{signature(from = "MCMClist", to = "dcCodaMCMC")}: ... }
    \item{coerce}{\code{signature(from = "MCMClist", to = "dcmle")}: ... }
    \item{confint}{\code{signature(object = "MCMClist")}: ... }
    \item{crosscorr.plot}{\code{signature(x = "MCMClist")}: ... }
    \item{crosscorr}{\code{signature(x = "MCMClist")}: ... }
    \item{densityplot}{\code{signature(x = "MCMClist")}: ... }
    \item{densplot}{\code{signature(x = "MCMClist")}: ... }
    \item{frequency}{\code{signature(x = "MCMClist")}: ... }
    \item{gelman.diag}{\code{signature(x = "MCMClist")}: ... }
    \item{gelman.plot}{\code{signature(x = "MCMClist")}: ... }
    \item{geweke.diag}{\code{signature(x = "MCMClist")}: ... }
    \item{heidel.diag}{\code{signature(x = "MCMClist")}: ... }
    \item{lambdamax.diag}{\code{signature(x = "MCMClist")}: ... }
    \item{mcpar}{\code{signature(x = "MCMClist")}: ... }
    \item{nchain}{\code{signature(x = "MCMClist")}: ... }
    \item{niter}{\code{signature(x = "MCMClist")}: ... }
    \item{nvar}{\code{signature(x = "MCMClist")}: ... }
    \item{pairs}{\code{signature(x = "MCMClist")}: ... }
    \item{plot}{\code{signature(x = "MCMClist", y = "missing")}: ... }
    \item{qqmath}{\code{signature(x = "MCMClist")}: ... }
    \item{quantile}{\code{signature(x = "MCMClist")}: ... }
    \item{raftery.diag}{\code{signature(x = "MCMClist")}: ... }
    \item{thin}{\code{signature(x = "MCMClist")}: ... }
    \item{traceplot}{\code{signature(x = "MCMClist")}: ... }
    \item{varnames}{\code{signature(x = "MCMClist")}: ... }
    \item{xyplot}{\code{signature(x = "MCMClist")}: ... }
	 }
}
\author{
Peter Solymos
}
\seealso{
\code{\link[coda]{mcmc.list}}
}
\examples{
showClass("MCMClist")
}
\keyword{classes}
