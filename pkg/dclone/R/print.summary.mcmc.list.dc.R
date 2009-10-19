print.summary.mcmc.dc <-
function(x, digits = max(3, .Options$digits - 3), ...)
{
    cat("\n", "Iterations = ", x$start, ":", x$end, "\n", sep = "")
    cat("Thinning interval =", x$thin, "\n")
    cat("Number of chains =", x$nchain, "\n")
    cat("Sample size per chain =", (x$end - x$start)/x$thin + 1, "\n")
    cat("Number of clones =", x$nclones)
    if (!is.null(x$converged))
        cat(paste(" (DC ", ifelse(x$converged, "", "not "), "converged)", sep=""))
    cat("\n")
    cat("\n1. Empirical mean and standard deviation for each variable,")
    cat("\n   plus standard error of the mean:\n\n")
    print(x$statistics, digits = digits, ...)
    cat("\n2. Quantiles for each variable:\n\n")
    print(x$quantiles, digits = digits, ...)
    cat("\n")
    invisible(x)
}
