.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                    fields=c("Version", "Date"))
    packageStartupMessage(paste(pkgname, ver[1], "\t", ver[2]))
    invisible(NULL)
}

.onLoad <- function(libname, pkgname){
    if (is.null(getOption("rspf.optim.control")))
        options("rspf.optim.control"=list(maxit = 20000))
    if (is.null(getOption("rspf.robust.probit")))
        options("rspf.robust.probit"=FALSE)
    invisible(NULL)
}

.onUnload <- function(libpath){
    options("rspf.optim.control"=NULL)
    options("rspf.robust.probit"=NULL)
    invisible(NULL)
}

