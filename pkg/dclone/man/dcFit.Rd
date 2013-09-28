\encoding{UTF-8}
\name{.dcFit}
\Rdversion{1.1}
\alias{.dcFit}
\title{
Internal function for iterative model fitting with data cloning
}
\description{
This is the workhorse for \code{\link{dc.fit}} and 
\code{\link{dc.parfit}}.
}
\usage{
.dcFit(data, params, model, inits, n.clones, 
    multiply = NULL, unchanged = NULL, 
    update = NULL, updatefun = NULL, initsfun = NULL, 
    flavour = c("jags", "bugs"), 
    n.chains=3, cl = NULL, parchains = FALSE, ...)
{
}
\arguments{
  \item{data}{
A named list (or environment) containing the data.
}
  \item{params}{
Character vector of parameters to be sampled.
It can be a list of 2 vectors, 1st element
is used as parameters to monitor, the 2nd is used
as parameters to use in calculating the data cloning
diagnostics.
}
  \item{model}{
Character string (name of the model file), a function containing 
the model, or a \code{\link{custommodel}} object (see Examples).
}
  \item{inits}{
Optional specification of initial values in the form of a list or a 
function (see Initialization at \code{\link[rjags]{jags.model}}). 
If missing, will be treated as \code{NULL} and initial values will 
be generated automatically.
}
  \item{n.clones}{
An integer vector containing the numbers of clones to use iteratively.
}
  \item{multiply}{
Numeric or character index for list element(s) in the \code{data} 
argument to be multiplied by the number of clones instead of repetitions.
}
  \item{unchanged}{
Numeric or character index for list element(s) in the \code{data} 
argument to be left unchanged.
}
  \item{update}{
Numeric or character index for list element(s) in the \code{data} 
argument that has to be updated by \code{updatefun} in each iterations. 
This usually is for making priors more informative, and enhancing 
convergence. See Details and Examples.
}
  \item{updatefun}{
A function to use for updating \code{data[[update]]}. 
It should take an 'mcmc.list' object as 1st argument,
2nd argument can be the number of clones.
See Details and Examples.
}
  \item{initsfun}{
A function to use for generating initial values, \code{inits} are 
updated by the object
returned by this function from the second iteration. If initial values
are not dependent on the previous iteration, this should be \code{NULL}, 
otherwise, it should take an 'mcmc.list' object as 1st argument, 
2nd argument can be the number of clones. 
This feature is useful if latent nodes are provided in \code{inits}
so it also requires to be cloned for subsequent iterations.
See Details and Examples.
}
  \item{flavour}{
  If \code{"jags"}, the function \code{\link{jags.fit}} is called. 
  If \code{"bugs"}, the function \code{\link{bugs.fit}} is called.
}
  \item{n.chains}{
Number of chains to generate.
}
  \item{cl}{
A cluster object created by \code{\link[parallel]{makeCluster}}, or
an integer, see \code{\link{parDosa}} and 
\code{\link{evalParallelArgument}}.
}
  \item{parchains}{
  Logical, whether parallel chanins should be run.
}
  \item{\dots}{
  Other values supplied to \code{\link{jags.fit}}, or 
  \code{\link{bugs.fit}}, depending on the \code{flavour} argument.
}
}
\value{
An object inheriting from the class 'mcmc.list'.
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}, 
implementation is based on many discussions 
with Khurram Nadeem and Subhash Lele.
}
\seealso{
\code{\link{dc.fit}}, \code{\link{dc.parfit}}
}
\keyword{ models }
\keyword{ htest }
