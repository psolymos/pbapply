print.lambdamax.diag <-
function (x, digits = max(3, getOption("digits") - 3), ...)
{
    npar <- attr(x, "npar")
    y <- x
    attributes(y) <- NULL
    if (npar > 1) {
        cat("Larges eigenvalue:\n")
    } else {
        cat("Standard deviation:\n")
    }
    cat(round(y, digits = digits), "\n")
    invisible(x)
}
