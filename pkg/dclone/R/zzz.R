.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                    fields=c("Version", "Date"))
    cat(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    if (is.null(getOption("dclone.crit"))) {
        options("dclone.crit"=list(r.hat=1.1))
    }
    if (is.null(getOption("dclone.cluster"))) {
        options("dclone.cluster"=list(load.balancing=TRUE))
    }
    invisible(NULL)
}

.onUnload <- function(libpath){
    options("dclone.crit"=NULL)
    options("dclone.cluster"=NULL)
    invisible(NULL)
}

