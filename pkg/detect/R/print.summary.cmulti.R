print.summary.cmulti <-
function (x, digits, ...) 
{
    if (missing(digits))
        digits <- max(3, getOption("digits") - 3)
    cat("\nCall:", deparse(x$call,
        width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")
    if (x$type == "dis")
        cat("Distance Sampling (half-normal, circular area)\n")
    if (x$type == "rem")
        cat("Removal Sampling (homogeneous singing rate)\n")
    if (x$type == "mix")
        cat("Removal Sampling (heterogeneous singing rate)\n")
    cat(paste("Conditional Maximum Likelihood estimates\n\n", sep = ""))
    cat(paste("Coefficients:\n", sep = ""))
    printCoefmat(x$coefficients, digits = digits, signif.legend = FALSE)
    if (!any(is.na(array(x$coefficients)))) {
        if (getOption("show.signif.stars") & any(x$coefficients[,4] < 0.1))
            cat("---\nSignif. codes: ", "0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", "\n")
    }
    cat("\nLog-likelihood:", formatC(x$loglik, digits = digits), 
        "\nBIC =", formatC(x$bic, digits = digits), "\n")
    cat("\n")
    invisible(x)
}
