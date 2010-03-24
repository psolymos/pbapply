`print.singleocc` <-
function (x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:", deparse(x$call,
        width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")
#    if (!x$converged)
#        cat("model did not converge\n")
    cat(paste("Single survey site-occupancy model", sep = ""))
    pen <- if (x$penalized)
        "Penalized " else ""
    cat(paste("\n", pen, "Maximum Likelihood estimates (", x$method, " method)\n\n", sep = ""))

    cat(paste("Coefficients for occurrence (", x$link["occ"], " link):\n", sep = ""))
    print.default(format(x$coefficients$occ, digits = digits),
        print.gap = 2, quote = FALSE)
    cat(paste("Coefficients for detection (", x$link["det"], " link):\n", sep = ""))
    print.default(format(x$coefficients$det, digits = digits),
        print.gap = 2, quote = FALSE)
    cat("\n")
    invisible(x)
}
