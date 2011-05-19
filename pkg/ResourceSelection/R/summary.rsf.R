summary.rsf <-
function (object, type, ...) 
{
    boot <- object$bootstrap
    if (missing(type)) {
        type <- if (is.null(boot))
        "mle" else "boot"
    }
    type <- match.arg(type, c("mle", "boot"))
    if (type == "boot" && is.null(boot))
        stop("cannot provide Bootstrap type summary")
    if (type != "boot")
        boot <- NULL

    k <- length(object$coefficients)
    coefs <- coef(object)
    se <- if (type == "boot")
        apply(boot, 1, sd) else object$std.error
    tstat <- coefs/se
#    pval <- 2 * pt(abs(tstat), object$df.residual, lower.tail = FALSE)
    ## z test because no overdspersion
    pval <- 2 * pnorm(-abs(tstat))
    coefs <- cbind(coefs, se, tstat, pval)
#    colnames(coefs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    colnames(coefs) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    coefs <- coefs[1:k, , drop = FALSE]
    rownames(coefs) <- names(coef(object, "sta"))

    if (identical(object$m, 0)) {
        omega <- sum(object$y)/length(object$y)
        alpha <- mean(fitted(object, "avail"))
        fit <- fitted(object, "all")
        p <- (omega * fit) / (omega * fit + (1-omega)*alpha)
        hl <- hoslem.test(object$y, p)
    } else hl <- NULL

    out <- list(call = object$call, coefficients=coefs, loglik = object$loglik,
        converged = object$converged, m = object$m, fitted.values = object$fitted.values,
        control = object$control, link = object$link, terms = object$terms,
        bootstrap=boot, type=type, B=ncol(object$bootstrap)-1, np=object$np, bic=BIC(object),
        hoslem.test=hl)
#    out$hoslem.test <- NULL
    class(out) <- "summary.rsf"
    return(out)
}

