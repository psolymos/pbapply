cmulti <- 
function(formula, data, type=c("rem", "mix", "dis"), 
inits=NULL, method="Nelder-Mead", ...)
{
    if (missing(data)) 
        data <- environment(formula)
    rv <- dFormula(formula, data, drop=FALSE)
    if (is.null(rv$D))
        stop("sampling design matrix not provided in LHS")
    if (!is.null(rv$Q) || !is.null(rv$Z))
        stop("RHS must have 1 part only")
    rv <- rv[!(names(rv) %in% c("Z", "Q"))]
    NAM <- colnames(rv$X)
    if (ncol(rv$X) < 2)
        rv$X <- NULL
    or <- cmulti.fit(rv$Y, rv$D, rv$X, type=type,
        inits=inits, method=method, ...)
    rv <- c(rv, or)
    rv$call <- match.call()
    rv$type <- type
    names(rv$coefficients) <- switch(type,
        "dis" = paste("log.tau", NAM, sep="_"),
        "mix" = c("log.phi", paste("logit.c", NAM, sep="_")),
        "rem" = paste("log.phi", NAM, sep="_"))
    class(rv) <- c("cmulti")
    rv
}
