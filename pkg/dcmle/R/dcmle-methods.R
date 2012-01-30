## methods
setMethod("show", "dcMle", function(object) {
    k <- object@n.clones
    if (is.null(k)) {
        print(summary(object@mcmc))
    } else {
        attributes(k) <- NULL
        n <- data.frame(start=object@start, end=object@end, thin=object@thin,
            n.iter=object@end-object@start+1,
            n.chains=object@n.chains, n.clones=k)
        digits <- max(3, getOption("digits") - 3)
        cat("Settings:\n")
        print(n, digits=digits, row.names=FALSE)
        cat("\nCoefficients:\n")
        printCoefmat(object@summary, digits = digits, signif.legend = TRUE)
        cat("\nConvergence:\n")
        print(object@dcdiag, digits=digits, row.names=FALSE)
    }
    invisible(object)
})
## show with title is done here
setGeneric("summary",
    function(object, ...)
        standardGeneric("summary")
)
setMethod("summary", "dcMle", function(object, title, ...) {
    if (missing(title))
        title <- paste("Object of class \"", class(object)[1L], "\"", sep="")
    cat(title, "\n\n")
    show(object)
    cat("\n")
    invisible(object)
})
## methods with generic defined in stats
setMethod("coef", "dcMle", function(object, ...) coef(object@mcmc, ...))
setMethod("vcov", "dcMle", function(object, ...) vcov(object@mcmc, ...))
setMethod("confint", "dcMle", function(object, parm, level = 0.95, ...) {
    if (!inherits(object@mcmc, "mcmc.list.dc"))
        stop("'confint' method not defined for k=1")
    confint(object@mcmc, parm, level, ...)
})
setMethod("quantile", "dcMle", function(x, ...) quantile(x@mcmc, ...))
## methods with generic defined in dclone
setMethod("dcdiag", "dcMle", function(x, ...) x@dcdiag)
setMethod("dctable", "dcMle", function(x, ...) x@dctable)
setMethod("dcsd", "dcMle", function(object, ...) dcsd(object@mcmc, ...))
setMethod("nclones", "dcMle", function(x, ...) x@n.clones)
## show for **Fit objects
setMethod("show", "dcFit", function(object) {
    str(object)
    invisible(object)
})
setMethod("show", "gsFit", function(object) {
    str(object)
    invisible(object)
})
