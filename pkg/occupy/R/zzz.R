.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                    fields=c("Version", "Date"))
    cat(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    if (is.null(getOption("occupy.control.optim"))) {
        options("occupy.control.optim"=list(maxit = 20000, fnscale = -1, reltol = 1e-08))
    }
    if (is.null(getOption("occupy.control.mcmc"))) {
        options("occupy.control.mcmc"=list(n.chains = 3, n.adapt = 1000, n.update = 4000, thin = 1, n.iter = 5000))
    }
    invisible(NULL)
}

.onUnload <- function(libpath){
    if (!is.null(getOption("occupy.control.optim"))) {
        options("occupy.control.optim"=NULL)
    }
    if (!is.null(getOption("occupy.control.mcmc"))) {
        options("occupy.control.mcmc"=NULL)
    }
    invisible(NULL)
}

