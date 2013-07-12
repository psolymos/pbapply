\encoding{UTF-8}
\name{DcloneEnv}
\Rdversion{1.1}
\alias{DcloneEnv}
\alias{.DcloneEnvModel}
\alias{.DcloneEnvResults}
\alias{pullDcloneEnv}
\alias{pushDcloneEnv}
\alias{clearDcloneEnv}
\alias{listDcloneEnv}
\alias{existsDcloneEnv}
\title{
Manipulating dclone environments
}
\description{
Manipulating dclone environments.
}
\usage{
pullDcloneEnv(x, type = c("model", "results"))
pushDcloneEnv(x, value, type = c("model", "results"))
clearDcloneEnv(..., list = character(), 
    type = c("model", "results"))
listDcloneEnv(type = c("model", "results"))
existsDcloneEnv(x, type = c("model", "results"),
    mode = "any", inherits = TRUE)
}
\arguments{
  \item{x}{
a variable name, given as a character string. 
No coercion is done, and the first element of a 
character vector of length greater than one will be used,
 with a warning.
}
  \item{value}{
a value to be assigned to \code{x}.
}
  \item{type}{
character, the type of environment to be accessed, see Details.
}
  \item{\dots}{
the objects to be removed, as names (unquoted) 
or character strings (quoted).
}
  \item{list}{
a character vector naming objects to be removed.
}
  \item{mode}{
the mode or type of object sought: see the \code{\link{exists}}.
}
  \item{inherits}{
logical, should the enclosing frames of the environment be searched?
}
}
\details{
\code{type = "model"} manipulates the \code{.DcloneEnvModel} 
environment, which is meant to store temporary objects
for model fitting with `snow' type parallelism
(see \code{\link{parDosa}} for the implementation).
This is swiped clean after use.

The\code{type = "results"} manipulates the \code{.DcloneEnvResults}
environment, which is meant to store result objects on the workers.
This is \emph{not} swiped clean after use.

\code{pullDcloneEnv} pulls an object from these environments,
similar to \code{\link{get}} in effect.

\code{pushDcloneEnv} pushes an object to these environments,
similar to \code{\link{assign}} in effect.

\code{clearDcloneEnv} removes object(s) from these environments,
similar to \code{\link{rm}} in effect.

\code{listDcloneEnv} lists name(s) of object(s) in these environments,
similar to \code{\link{ls}} in effect.

\code{existsDcloneEnv} tests if an object exists in these environments,
similar to \code{\link{exists}} in effect.
}
\value{
For \code{pullDcloneEnv}, the object found. 
If no object is found an error results.

\code{pushDcloneEnv} is invoked for its side effect, 
which is assigning \code{value} to the variable \code{x}.

For \code{clearDcloneEnv} its is the side effect of an object removed.
No value returned.

\code{listDcloneEnv} returns a character vector.

\code{existsDcloneEnv} returns logical, \code{TRUE} if and only if an
 object of the correct name and mode is found.
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link{parDosa}}
}
\keyword{ utilities }
\keyword{ manip }
\keyword{ environment }
