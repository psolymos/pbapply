confint.svabu <-
function (object, parm, level = 0.95, model = c("full", "sta", "det", "zif"), type, ...)
{
    model <- match.arg(model)
    if (!object$zeroinfl && model == "zif")
        stop("not ZIF component found")
    boot <- attr(object, "bootstrap")
    if (missing(type)) {
        type <- if (is.null(boot))
        "cmle" else "boot"
    }
    type <- match.arg(type, c("cmle", "boot"))
#    if (model == "zif" && type == "cmle" && is.null(object$phi.se))
#        stop("bootstrap is required to provide values for 'phi'")
    if (type == "boot" && is.null(boot))
        stop("no bootstrap results found")
    if (type != "boot")
        boot <- NULL

    cf <- coef(object, model)
#    if (model == "full" && type == "cmle" && is.null(object$phi.se))
#            cf <- cf[-length(cf)]
    pnames <- names(cf)
    if (missing(parm)) {
        parm <- pnames
    } else {
        if (is.numeric(parm))
            parm <- pnames[parm]
    }
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%", sep="")
    ci <- array(NA, dim = c(length(parm), 2), dimnames = list(parm, pct))
    if (type == "cmle") {
#        fac <- qt(a, object$df.residual)
        fac <- qnorm(a)
#        if (model == "phi") {
#            ses <- c(phi=object$phi.se)
#        } else {
#            ses <- sqrt(diag(vcov(object, model, type)))
#            if (model=="full" && !is.null(object$phi.se))
#                ses <- c(ses, phi=object$phi.se)
#        }
        ses <- sqrt(diag(vcov(object, model, type)))
        ci[] <- cf[parm] + ses[parm] %o% fac
    } else {
        nps <- sapply(object$coefficients, length)
        ip <- switch(model,
            "full" = 1:sum(nps),
            "sta" = 1:nps[1],
            "det" = (nps[1] + 1):(nps[1] + nps[2]),
            "zif" = (nps[1] + nps[2] + 1):sum(nps))
        cii <- t(apply(matrix(boot[ip, ], nrow=length(ip)), 1, quantile, probs=a))
        rownames(cii) <- pnames
        ci[] <- cii[parm,,drop=FALSE]
    }
    return(ci)
}

