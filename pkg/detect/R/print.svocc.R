print.svocc <-
function (x, digits, ...)
{
    if (missing(digits))
        digits <- max(3, getOption("digits") - 3)
    cat("\nCall:", deparse(x$call,
        width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")
#    if (!x$converged)
#        cat("model did not converge\n")
    cat(paste("Single visit site-occupancy model", sep = ""))
    pen <- if (x$penalized)
        "Penalized " else ""
    cat(paste("\n", pen, "Maximum Likelihood estimates (", x$method, " method)\n\n", sep = ""))

    cat(paste("Coefficients for occurrence (", x$link$sta, " link):\n", sep = ""))
    print.default(format(x$coefficients$sta, digits = digits),
        print.gap = 2, quote = FALSE)
    cat(paste("Coefficients for detection (", x$link$det, " link):\n", sep = ""))
    print.default(format(x$coefficients$det, digits = digits),
        print.gap = 2, quote = FALSE)
    Conv <- if (!x$penalized)
        x$converged[1] else x$converged[2]
    if (!Conv) 
        cat("Warning:\n  Model did not converge\n\n")
    cat("\n")
    invisible(x)
}

