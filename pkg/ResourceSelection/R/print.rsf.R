print.rsf <-
function (x, digits, ...)
{
    rspf <- if (x$link == "log")
        FALSE else TRUE
    if (missing(digits))
        digits <- max(3, getOption("digits") - 3)
    cat("\nCall:", deparse(x$call,
        width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")
    pp <- if (rspf)
        "Probability " else ""
    ppp <- " model"
    if (!rspf)
        ppp <- " (Exponential RSF) model"
    if (x$link == "logit")
        ppp <- " (Logistic RSPF) model"
    cat(paste("Resource Selection ", pp, "Function", ppp, sep = ""))
#    mc <- if (x$m != 0)
#        "\nMatched case " else "\nNon-matched case "
    mc <- if (identical(x$m, 0))
        "\nNon-matched Used-Available design\n" else "\nMatched Used-Available design\n"
    cat(paste(mc, "Maximum Likelihood estimates\n\n", sep = ""))
    cat(paste("Coefficients (", x$link, " link):\n", sep = ""))
#    cfs <- if (rspf)
#        x$coefficients else x$coefficients[-1]
    print.default(format(x$coefficients, digits = digits),
        print.gap = 2, quote = FALSE)
    if (!x$converged) 
        cat("Warning:\n  Model did not converge\n")
    cat("\n")
    invisible(x)
}

