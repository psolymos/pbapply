print.cmulti <-
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
    print.default(format(x$coefficients, digits = digits),
        print.gap = 2, quote = FALSE)
    cat("\n")
    invisible(x)
}
