update.svisit <-
function (object, formula., ..., evaluate = TRUE, control) 
{
    if (missing(control))
        control <- object$control
    if (!evaluate)
        control <- NULL
    if (!is.null(control)) {
        ## saving current settings
        old.optim.control <- getOption("detect.optim.control")
        old.optim.method <- getOption("detect.optim.method")
        old.dc.control <- getOption("detect.dc.control")
        old.mcmc.control <- getOption("detect.mcmc.control")
        ## applying controls from fit, restoring settings
        if (!is.null(control$optim.control)) {
            options("detect.optim.control"=control$optim.control)
            on.exit(options("detect.optim.control"=old.optim.control), add=TRUE)
        }
        if (!is.null(control$optim.method)) {
            options("detect.optim.method"=control$optim.method)
            on.exit(options("detect.optim.method"=old.optim.method), add=TRUE)
        }
        if (!is.null(control$dc.control)) {
            options("detect.dc.control"=control$dc.control)
            on.exit(options("detect.dc.control"=old.dc.control), add=TRUE)
        }
        if (!is.null(control$mcmc.control)) {
            options("detect.mcmc.control"=control$mcmc.control)
            on.exit(options("detect.mcmc.control"=old.mcmc.control), add=TRUE)
        }
    }

    call <- object$call
    if (is.null(call)) 
        stop("need an object with call component")
    extras <- match.call(expand.dots = FALSE)$...
    if (!missing(formula.)) 
        call$formula <- update.formula.svisit(formula(object)$full, formula.)
    if (length(extras) > 0) {
        existing <- !is.na(match(names(extras), names(call)))
        for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
        if (any(!existing)) {
            call <- c(as.list(call), extras[!existing])
            call <- as.call(call)
        }
    }
    if (evaluate) 
        eval(call, parent.frame())
    else call
}

