.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                    fields=c("Version", "Date"))
    cat(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    ## dclone options setup
    if (is.null(getOption("dclone.rhat")))
        options("dclone.rhat"=1.1)
    if (is.null(getOption("dclone.autoburnin")))
        options("dclone.autoburnin"=TRUE)
    if (is.null(getOption("dclone.diag")))
        options("dclone.diag"=0.05)
    if (is.null(getOption("dclone.verbose")))
        options("dclone.verbose"=1)
    if (is.null(getOption("dclone.LB")))
        options("dclone.LB"=FALSE)
    if (is.null(getOption("dclone.LB")))
        options("dclone.RNG"="RNGstream")
    if (as.numeric(substr(utils::packageDescription("rjags", 
        field="Version"), 1, 1)) > 1)
            load.module("glm")
    invisible(NULL)
}

.onUnload <- function(libpath){
    ## remove dclone options
    options("dclone.rhat"=NULL)
    options("dclone.autoburnin"=NULL)
    options("dclone.diag"=NULL)
    options("dclone.verbose"=NULL)
    options("dclone.LB"=NULL)
    options("dclone.RNG"=NULL)
    invisible(NULL)
}

