svocc <-
function (formula, data, link.sta = "cloglog", link.det = "logit", penalized = FALSE,
    method = c("optim", "dc"), inits,
    model = TRUE, x = FALSE, cl=NULL, ...)
{
    ## parsing the formula
    if (missing(data))
        data <- NULL
#    out <- svisitFormula(formula, data, n=sys.nframe()-1) ## parent frame
    out <- svisitFormula(formula, data, n=0) ## global environment
    out$call = match.call()
#    out$y <- NULL
    Y <- out$y
    X <- out$x$sta
    Z <- out$x$det
    Xlevels <- out$levels

    ## check variables
    if (length(Y) < 1) 
        stop("empty model")
    if (all(Y > 0)) 
        stop("invalid dependent variable, no zero value")
    if (!isTRUE(all.equal(as.vector(Y), as.integer(round(Y + 
        0.001))))) 
        stop("invalid dependent variable, non-integer values")
    Y <- as.integer(round(Y + 0.001))
    if (any(Y < 0)) 
        stop("invalid dependent variable, negative counts")
    if (any(!(Y %in% c(0, 1)))) 
        stop("invalid dependent variable, not in c(0, 1)")
    if (setequal(colnames(Z), colnames(X))) 
        stop("at least one covariate should be separate for occupancy and detection parts of the formula")
    if (length(Y) != NROW(X)) 
        stop("invalid dependent variable, not a vector")
    if (all(union(colnames(X), colnames(Z))[-1] %in% names(unlist(Xlevels))))
        stop("model must include at least one continuous covariate")

    ## occ and det linkinv functions
    links <- c("logit", "probit", "cloglog")
    link.sta <- match.arg(link.sta, links)
    link.det <- match.arg(link.det, links)
    ## method
    method <- match.arg(method)
    ## fit
    fit <- svocc.fit(Y, X, Z, link.sta, link.det, penalized = penalized, auc = FALSE, 
        method=method, inits=inits, cl=cl, ...)
    ## return value
    out <- c(fit, out)
    if (!model) 
        out$model <- NULL
    if (!x) 
        out$x <- NULL
    class(out) <- c("svocc", "svisit")
    return(out)
}

