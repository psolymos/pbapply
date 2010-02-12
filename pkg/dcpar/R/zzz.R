.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                    fields=c("Version", "Date"))
    cat(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    if (is.null(getOption("dcpar"))) {
        options("dcpar.LB"=FALSE)
    }
    invisible(NULL)
}

.onUnload <- function(libpath){
    options("dcpar.LB"=NULL)
    invisible(NULL)
}

