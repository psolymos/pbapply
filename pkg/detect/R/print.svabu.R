print.svabu <-
function (x, digits, ...)
{
    statename <- if (all(x$area == 1))
        "abundance" else "density"
    if (missing(digits))
        digits <- max(3, getOption("digits") - 3)
    cat("\nCall:", deparse(x$call,
        width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")
    modt <- if (x$zeroinfl)
        "Zero Inflated " else ""
    modd <- if (inherits(x, "svabu_p"))
        "Poisson model" else "Negative Binomial model"
    cat(paste("Single visit Binomial - ", modt, modd, sep = ""))
    cat(paste("\nConditional Maximum Likelihood estimates\n\n", sep = ""))

    cat(paste("Coefficients for ", statename," (", x$link$sta, " link):\n", sep = ""))
    print.default(format(x$coefficients$sta, digits = digits),
        print.gap = 2, quote = FALSE)
    cat(paste("Coefficients for detection (", x$link$det, " link):\n", sep = ""))
    print.default(format(x$coefficients$det, digits = digits),
        print.gap = 2, quote = FALSE)
    if (x$zeroinfl) {
        cat(paste("Coefficients for zero inflation (", x$link$zif, " link):\n", sep = ""))
        print.default(format(x$coefficients$zif, digits = digits),
            print.gap = 2, quote = FALSE)
    }
    if (!x$converged) 
        cat("Warning:\n  Model did not converge\n")
    cat("\n")
    invisible(x)
}

