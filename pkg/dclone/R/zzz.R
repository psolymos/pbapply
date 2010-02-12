.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                    fields=c("Version", "Date"))
    cat(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    ## dclone options setup
    if (is.null(getOption("dclone"))) {
        options("dclone.rhat"=1.1)
        options("dclone.autoburnin"=TRUE)
        options("dclone.diag"=0.05)
        options("dclone.verbose"=1)
#        dcoptions <- list(r.hat = list(crit = 1.1, autoburnin = TRUE),
#            dcdiag = list(crit = 0.05),
#            verbose = 1)
#        options("dclone"=dcoptions)
    }
    invisible(NULL)
}

.onUnload <- function(libpath){
#    options("dclone"=NULL)
    options("dclone.rhat"=NULL)
    options("dclone.autoburnin"=NULL)
    options("dclone.diag"=NULL)
    options("dclone.verbose"=NULL)
    invisible(NULL)
}

