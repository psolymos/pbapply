\name{wait}
\Rdversion{1.1}
\alias{wait}
\title{
Function to wait for a key.
}
\description{
Wait for a key and evaluates an expression.
}
\usage{
wait(ask = "Press any key to continue ... ", do = NULL)
}
\arguments{
  \item{ask}{
A text message to print on the screen before evaluation of the expression.
}
  \item{do}{
An expression to evaluate.
}
}
\value{
If \code{ask = NULL} waits for a key to press, otherwise
prints the text \code{ask}, waits for a key to press
and subsequently evaluates \code{do} if that is not \code{NULL}.
}
\author{
Peter Solymos
}
\examples{
\dontrun{
wait()
wait("What's up, doc? ")
\dontrun{
wait("Opening 'dclone' demo\nPress any key ... ", demo("dclone"))
}
}
}
\keyword{ IO }
