.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                    fields=c("Version", "Date"))
    cat(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    if (is.null(getOption("occupy.optim.control")))
        options("occupy.optim.control"=list(maxit = 20000, fnscale = -1, reltol = 1e-08))
    if (is.null(getOption("occupy.optim.method")))
        options("occupy.optim.method"="BFGS")
    if (is.null(getOption("occupy.mcmc.control")))
        options("occupy.mcmc.control"=list(n.chains = 3, n.adapt = 1000, n.update = 4000, thin = 1, n.iter = 5000, prec = 0.1))
    invisible(NULL)
}

.onUnload <- function(libpath){
    options("occupy.optim.control"=NULL)
    options("occupy.optim.method"=NULL)
    options("occupy.mcmc.control"=NULL)
    invisible(NULL)
}

