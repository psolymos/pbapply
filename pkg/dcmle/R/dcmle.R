## wrapper function
## calls the interal .dcmle function and coerces to dcmle S4 class
dcmle <- function(x, params, n.clones=1, cl=NULL, nobs, ...) {
    out <- as(dcmle:::.dcmle(x=x, params=params, 
        n.clones=n.clones, cl=cl, nobs=nobs, ...), "dcmle")
    out@call <- match.call()
    if (!missing(nobs))
        out@nobs <- nobs
    out
}
## internal workhorse returning S3 mcmc.list object
## this still has updated.model just in case...
.dcmle <- function(x, params, n.clones=1, cl=NULL, nobs, ...) {
    ## get defaults right for cl argument
    if (!is.null(cl) && !missing(cl))
        if (!inherits(cl, "cluster") && cl == 1)
            cl <- NULL
    cl <- evalParallelArgument(cl)
    ## coerce into dcFit (issue error if it is not possible)
    x <- as(x, "dcFit")
    ## gete params if not defined by argument
    if (missing(params))
        params <- x@params
    if (is.null(params))
        stop("'params' must be provided")
    ## make a copy of input model
    model <- x@model
    ## single model
    if (length(n.clones) == 1) {
        ## make a copy of input inits
        inits <- x@inits
        ## clone the inits if k>1 (latent variables might require this)
        if (n.clones > 1 && !is.null(x@initsfun)) {
            initsfun <- match.fun(x@initsfun)
            ARGS <- names(formals(initsfun))
            ARGS <- ARGS[1:max(2, length(ARGS))]
            if (length(ARGS)>2)
                stop("too long argument list for 'initsfun'")
            if (length(ARGS)==2)
                inits <- eval(local(parse(text = paste("inits <- ", 
                    deparse(substitute(initsfun)), "(", 
                    ARGS[2], "=n.clones)", sep = ""))))
        }
        ## clone the data
        dat <- dclone(x@data, n.clones, x@multiply, x@unchanged)
        ## fit the model according to input flavour and cl argument
        if (x@flavour == "jags" && is.null(cl))
            out <- jags.fit(dat, params, model, inits, ...)
        if (x@flavour == "bugs" && is.null(cl))
            out <- bugs.fit(dat, params, model, inits, ...)
        if (x@flavour == "jags" && !is.null(cl))
            out <- jags.parfit(cl, dat, params, x@model, inits, ...)
        if (x@flavour == "bugs" && !is.null(cl))
            stop("parallel chains with flavour='bugs' not supported")
    ## sequential model fit
    } else {
        ## sequential execution
        if (is.null(cl)) {
            out <- dc.fit(x@data, params, model, x@inits, 
                n.clones = n.clones, 
                multiply = x@multiply, 
                unchanged = x@unchanged,
                update = x@update,
                updatefun = x@updatefun,
                initsfun = x@initsfun,
                flavour = x@flavour, ...)
        ## parallel execution
        } else {
            out <- dc.parfit(cl, x@data, params, model, x@inits, 
                n.clones = n.clones, 
                multiply = x@multiply, 
                unchanged = x@unchanged,
                update = x@update,
                updatefun = x@updatefun,
                initsfun = x@initsfun,
                flavour = x@flavour, ...)
        }
    }
    out
}

