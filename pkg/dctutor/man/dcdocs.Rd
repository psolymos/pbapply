\encoding{UTF-8}
\name{dcdocs}
\alias{dcdocs}
\title{Display Package Documentation}
\description{
  Display package documentation using \code{pager} or \code{pdfviewer}
  defined in \code{\link{options}}.
}
\usage{
dcdocs(doc = c("dclone","dcglm","dcpar"), ext = ".pdf")
}
\arguments{
  \item{doc}{The name of the document (partial match, case sensitive). }
  \item{ext}{Character, file extension.}
}
\author{Peter Solymos \email{solymos@ualberta.ca, adapted Jari Oksanen's \code{\link[vegan]{vegandocs}}}}
\examples{
\dontrun{
dcdocs("dclone")
}
}
\keyword{ documentation }
