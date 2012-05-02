\name{dcmle-class}
\Rdversion{1.1}
\docType{class}
\alias{dcmle-class}
\alias{[,dcmle-method}
\alias{[[,dcmle-method}
\alias{acfplot,dcmle-method}
\alias{as.array,dcmle-method}
\alias{as.matrix,dcmle-method}
\alias{as.mcmc.list,dcmle-method}
\alias{autocorr.diag,dcmle-method}
\alias{chanames,dcmle-method}
\alias{chisq.diag,dcmle-method}
\alias{coef,dcmle-method}
\alias{coerce,dcmle,codaMCMC-method}
\alias{coerce,dcmle,dcCodaMCMC-method}
\alias{coerce,dcmle,MCMClist-method}
\alias{coerce,MCMClist,dcmle-method}
\alias{confint,dcmle-method}
\alias{crosscorr.plot,dcmle-method}
\alias{crosscorr,dcmle-method}
\alias{dcdiag,dcmle-method}
\alias{dcsd,dcmle-method}
\alias{dctable,dcmle-method}
\alias{densityplot,dcmle-method}
\alias{densplot,dcmle-method}
\alias{end,dcmle-method}
\alias{frequency,dcmle-method}
\alias{gelman.diag,dcmle-method}
\alias{geweke.diag,dcmle-method}
\alias{head,dcmle-method}
\alias{heidel.diag,dcmle-method}
\alias{lambdamax.diag,dcmle-method}
\alias{mcpar,dcmle-method}
\alias{nchain,dcmle-method}
\alias{nclones,dcmle-method}
\alias{niter,dcmle-method}
\alias{nvar,dcmle-method}
\alias{pairs,dcmle-method}
\alias{plot,dcmle,missing-method}
\alias{qqmath,dcmle-method}
\alias{quantile,dcmle-method}
\alias{raftery.diag,dcmle-method}
\alias{show,dcmle-method}
\alias{stack,dcmle-method}
\alias{start,dcmle-method}
\alias{str,dcmle-method}
\alias{summary,dcmle-method}
\alias{tail,dcmle-method}
\alias{thin,dcmle-method}
\alias{time,dcmle-method}
\alias{traceplot,dcmle-method}
\alias{update,dcmle-method}
\alias{varnames,dcmle-method}
\alias{vcov,dcmle-method}
\alias{window,dcmle-method}
\alias{xyplot,dcmle-method}

\title{Class \code{"dcmle"}}
\description{
Fitted model object from \code{\link{dcmle}}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("dcmle", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{call}:}{Object of class \code{"language"}, the call.}
    \item{\code{coef}:}{Object of class \code{"numeric"},
        coefficients (posterior means).}
    \item{\code{fullcoef}:}{Object of class \code{"numeric"},
        full coefficients, possibly with fixed values.}
    \item{\code{vcov}:}{Object of class \code{"matrix"},
        variance covariance matrix.}
    \item{\code{details}:}{Object of class \code{"dcCodaMCMC"},
        the fitted model object.}
    \item{\code{nobs}:}{Object of class \code{"integer"},
        number of observations, optional.}
    \item{\code{method}:}{Object of class \code{"character"}.}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "dcmle")}: ... }
    \item{[[}{\code{signature(x = "dcmle")}: ... }
    \item{acfplot}{\code{signature(x = "dcmle")}: ... }
    \item{as.array}{\code{signature(x = "dcmle")}: ... }
    \item{as.matrix}{\code{signature(x = "dcmle")}: ... }
    \item{as.mcmc.list}{\code{signature(x = "dcmle")}: ... }
    \item{autocorr.diag}{\code{signature(mcmc.obj = "dcmle")}: ... }
    \item{chanames}{\code{signature(x = "dcmle")}: ... }
    \item{chisq.diag}{\code{signature(x = "dcmle")}: ... }
    \item{coef}{\code{signature(object = "dcmle")}: ... }
    \item{coerce}{\code{signature(from = "codaMCMC", to = "dcmle")}: ... }
    \item{coerce}{\code{signature(from = "dcCodaMCMC", to = "dcmle")}: ... }
    \item{coerce}{\code{signature(from = "dcmle", to = "codaMCMC")}: ... }
    \item{coerce}{\code{signature(from = "dcmle", to = "dcCodaMCMC")}: ... }
    \item{coerce}{\code{signature(from = "dcmle", to = "MCMClist")}: ... }
    \item{coerce}{\code{signature(from = "MCMClist", to = "dcmle")}: ... }
    \item{confint}{\code{signature(object = "dcmle")}: ... }
    \item{crosscorr.plot}{\code{signature(x = "dcmle")}: ... }
    \item{crosscorr}{\code{signature(x = "dcmle")}: ... }
    \item{dcdiag}{\code{signature(x = "dcmle")}: ... }
    \item{dcsd}{\code{signature(object = "dcmle")}: ... }
    \item{dctable}{\code{signature(x = "dcmle")}: ... }
    \item{densityplot}{\code{signature(x = "dcmle")}: ... }
    \item{densplot}{\code{signature(x = "dcmle")}: ... }
    \item{end}{\code{signature(x = "dcmle")}: ... }
    \item{frequency}{\code{signature(x = "dcmle")}: ... }
    \item{gelman.diag}{\code{signature(x = "dcmle")}: ... }
    \item{geweke.diag}{\code{signature(x = "dcmle")}: ... }
    \item{head}{\code{signature(x = "dcmle")}: ... }
    \item{heidel.diag}{\code{signature(x = "dcmle")}: ... }
    \item{lambdamax.diag}{\code{signature(x = "dcmle")}: ... }
    \item{mcpar}{\code{signature(x = "dcmle")}: ... }
    \item{nchain}{\code{signature(x = "dcmle")}: ... }
    \item{nclones}{\code{signature(x = "dcmle")}: ... }
    \item{niter}{\code{signature(x = "dcmle")}: ... }
    \item{nvar}{\code{signature(x = "dcmle")}: ... }
    \item{pairs}{\code{signature(x = "dcmle")}: ... }
    \item{plot}{\code{signature(x = "dcmle", y = "missing")}: ... }
    \item{qqmath}{\code{signature(x = "dcmle")}: ... }
    \item{quantile}{\code{signature(x = "dcmle")}: ... }
    \item{raftery.diag}{\code{signature(x = "dcmle")}: ... }
    \item{show}{\code{signature(object = "dcmle")}: ... }
    \item{stack}{\code{signature(x = "dcmle")}: ... }
    \item{start}{\code{signature(x = "dcmle")}: ... }
    \item{str}{\code{signature(object = "dcmle")}: ... }
    \item{summary}{\code{signature(object = "dcmle")}: ... }
    \item{tail}{\code{signature(x = "dcmle")}: ... }
    \item{thin}{\code{signature(x = "dcmle")}: ... }
    \item{time}{\code{signature(x = "dcmle")}: ... }
    \item{traceplot}{\code{signature(x = "dcmle")}: ... }
    \item{update}{\code{signature(object = "dcmle")}: ... }
    \item{varnames}{\code{signature(x = "dcmle")}: ... }
    \item{vcov}{\code{signature(object = "dcmle")}: ... }
    \item{window}{\code{signature(x = "dcmle")}: ... }
    \item{xyplot}{\code{signature(x = "dcmle")}: ... }
	 }
}
\author{
Peter Solymos
}
\seealso{
\code{\link{dcmle}}
}
\examples{
showClass("dcmle")
}
\keyword{classes}
