print.lambdamax.diag <-
function (x, digits = max(3, getOption("digits") - 3), ...)
{
#    npar <- attr(x, "npar")
    y <- x
    attributes(y) <- NULL
#    if (npar > 1) {
#        cat("Largest eigenvalue = ")
#    } else {
#        cat("Standard deviation = ")
#    }
#    cat(round(y, digits = digits), "\n")
    cat("Largest eigenvalue = ", round(y, digits = digits), "\n")
    if (!is.null(nclones(x)))
        cat("Number of clones =", nclones(x), "\n")
    invisible(x)
}
