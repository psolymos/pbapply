\encoding{UTF-8}
\name{dcdocs}
\alias{dcdocs}
\title{Display Package Documentation}
\description{
  Display package documentation using \code{pager} or \code{pdfviewer}
  defined in \code{\link{options}}.
}
\usage{
dcdocs(doc = c("dclone","dcglm","dcpar"), pdf = TRUE)
}
\arguments{
  \item{doc}{The name of the document (partial match, case sensitive). }
  \item{pdf}{Logical, if the file is of PDF format (attaches the extension \code{.doc} to \code{doc}).}
}
\author{Peter Solymos \email{solymos@ualberta.ca, adapted Jari Oksanen's \code{\link[vegan]{vegandocs}}}}
\examples{
\dontrun{
dcdocs("dclone")
}
}
\keyword{ documentation }
