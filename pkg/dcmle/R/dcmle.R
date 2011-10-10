## wrapper function
dcmle <- function(x, params, n.clones=1, cl=NULL, ...) {
    x <- as(x, "dcFit")
    if (missing(params))
        params <- x@params
    model <- x@model
#    model <- structure(x@model, class="custommodel")
    if (length(n.clones) == 1) {
        inits <- x@inits
        if (n.clones > 1 && !is.null(x@initsfun)) {
            initsfun <- match.fun(x@initsfun)
            ARGS <- names(formals(initsfun))
            ARGS <- ARGS[1:max(2, length(ARGS))]
            if (length(ARGS)==2)
                eval(parse(text = paste("inits <- ", 
                    deparse(substitute(initsfun)), "(", 
                    ARGS[2], "=n.clones)", sep = "")))
        }
        dat <- dclone(x@data, n.clones, x@multiply, x@unchanged)
        if (x@flavour == "jags" && is.null(cl))
            out <- jags.fit(dat, params, model, inits, ...)
        if (x@flavour == "bugs" && is.null(cl))
            out <- bugs.fit(dat, params, model, inits, ...)
        if (x@flavour == "jags" && !is.null(cl))
            out <- jags.parfit(cl, dat, params, x@model, inits, ...)
        if (x@flavour == "bugs" && !is.null(cl))
            stop("parallel chains with flavour='bugs' not supported")
    } else {
        if (is.null(cl)) {
            out <- dc.fit(x@data, params, model, x@inits, 
                n.clones = n.clones, 
                multiply = x@multiply, 
                unchanged = x@unchanged,
                update = x@update,
                updatefun = x@updatefun,
                initsfun = x@initsfun,
                flavour = x@flavour, ...)
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
    as(out, "dcMle")
}
