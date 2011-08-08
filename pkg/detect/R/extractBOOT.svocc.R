extractBOOT.svocc <-
function(object, model = c("full", "sta", "det"), ...)
{
    tmp <- attr(object, "bootstrap")
    if (is.null(tmp))
        return(NULL)
    model <- match.arg(model)
    cf <- coef(object, model)
    wi <- seq(along = object$coefficients$sta)
    cfs <- rowMeans(tmp)
    ses <- apply(tmp, 1, sd)
    vcv <- cov(t(tmp))
    cors <- cor(t(tmp))
    if (model == "sta") {
        cfs <- cfs[wi]
        ses <- ses[wi]
        vcv <- data.matrix(vcv[wi, wi])
        cors <- data.matrix(cors[wi, wi])
    }
    if (model == "det") {
        cfs <- cfs[-wi]
        ses <- ses[-wi]
        vcv <- data.matrix(vcv[-wi, -wi])
        cors <- data.matrix(cors[-wi, -wi])
    }
    names(cfs) <- names(cfs) <- names(cf)
    names(ses) <- names(ses) <- names(cf)
    colnames(vcv) <- rownames(vcv) <- names(cf)
    colnames(cors) <- rownames(cors) <- names(cf)
    list(coefficients=cfs, std.error=ses, vcov=vcv, cor=cors,
        B = ncol(tmp) - 1, type = attr(tmp, "type"))
}

