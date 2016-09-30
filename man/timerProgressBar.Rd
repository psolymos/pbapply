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
the style taking values between 1 and 6.
1: progress bar with elapsed and remaining time,
remaining percentage is indicated by spaces between pipes
(default for this function),
2: throbber with elapsed and remaining time,
3: progress bar with remaining time printing elapsed time at the end,
remaining percentage is indicated by spaces between pipes
(default for \code{style} option in \code{\link{pboptions}}),
4: throbber with remaining time printing elapsed time at the end,
5: progress bar with elapsed and remaining time,
remaining percentage is indicated by dashes between square brackets,
6: progress bar with remaining time printing elapsed time at the end,
remaining percentage is indicated by dashes between square brackets.
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
test_fun <- function(...)
{
    pb <- timerProgressBar(...)
    on.exit(close(pb))
    for(i in seq(0, 1, 0.05)) {
        Sys.sleep(0.1)
        setTimerProgressBar(pb, i)
    }
    invisible(NULL)
}
## throbber with elapsed and remaining time
test_fun(style = 2)
## progress bar with remaining time
test_fun(width = 50, char = ".", style = 3)
test_fun(width = 50, char = ".", style = 6)
}
\keyword{ utilities }
