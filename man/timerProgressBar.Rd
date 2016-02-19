\name{timerProgressBar}
\alias{timerProgressBar}
\alias{setTimerProgressBar}
\alias{getTimerProgressBar}
\title{
Timer Progress Bar
}
\description{
Text progress bar with timer in the R console.
}
\usage{
timerProgressBar(min = 0, max = 1, initial = 0, char = "=",
    width = NA, title, label, style = 1, file = "")
getTimerProgressBar(pb)
setTimerProgressBar(pb, value, title = NULL, label = NULL)
}
\arguments{
  \item{min, max}{
(finite) numeric values for the extremes of the progress bar.
Must have \code{min} < \code{max}.
}
  \item{initial, value}{
initial or new value for the progress bar.
See Details for what happens with invalid values.
}
  \item{char}{
he character (or character string) to form the progress bar.
If number of characters is >1, it is silently stripped to length 1.
}
  \item{width}{
the width of the progress bar, as a multiple of the width of char.
If \code{NA}, the default, the number of characters is that
which fits into \code{getOption("width")}.
}
  \item{style}{
the style of the bar, currently ignored (for possible future use).
}
  \item{file}{
an open connection object or \code{""} which indicates the console.
}
  \item{pb}{
an object of class \code{"timerProgressBar"}.
}
  \item{title, label}{
ignored, for compatibility with other progress bars.
}
}
\details{
\code{timerProgressBar} will display a progress bar on the R console
(or a connection) via a text representation.

\code{setTimerProgessBar} will update the value. Missing (\code{NA}) and out-of-range values of value will be (silently) ignored. (Such values of \code{initial}
cause the progress bar not to be displayed until a valid value is set.)

The progress bar should be closed when finished with: this outputs the final newline character (see \code{\link{closepb}}).
}
\value{
For \code{timerProgressBar} an object of class \code{"timerProgressBar"}
inheriting from \code{"txtProgressBar"}.

For \code{getTimerProgressBar} and \code{setTimerProgressBar},
a length-one numeric vector giving the previous
value (invisibly for \code{setTimerProgressBar}).
}
\author{
Zygmunt Zawadzki <zawadzkizygmunt@gmail.com>

Peter Solymos <solymos@ualberta.ca>
}
\seealso{
The \code{timerProgressBar} implementation
follows closely the code of \code{\link[utils]{txtProgressBar}}.
}
\examples{
testit <- function(...)
{
    pb <- timerProgressBar(...)
    for(i in seq(0, 1, 0.05)) {
        Sys.sleep(0.2)
        setTimerProgressBar(pb, i)
    }
    Sys.sleep(0.2)
    close(pb)
}
testit()
}
\keyword{ utilities }
