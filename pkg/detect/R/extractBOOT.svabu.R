extractBOOT.svabu <-
function(object, model = c("full", "sta", "det", "phi"), ...)
{
    tmp <- attr(object, "bootstrap")
    if (is.null(tmp))
        return(NULL)
    model <- match.arg(model)
    if (!object$zeroinfl && model == "phi")
        stop("not ZI model, can't provide values for 'phi'")
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
        cfs <- cfs[-c(wi, nrow(tmp))]
        ses <- ses[-c(wi, nrow(tmp))]
        vcv <- data.matrix(vcv[-c(wi, nrow(tmp)), -c(wi, nrow(tmp))])
        cors <- data.matrix(cors[-c(wi, nrow(tmp)), -c(wi, nrow(tmp))])
    }
    if (model == "phi") {
        cfs <- cfs[nrow(tmp)]
        ses <- ses[nrow(tmp)]
        vcv <- data.matrix(vcv[nrow(tmp), nrow(tmp)])
        cors <- data.matrix(cors[nrow(tmp), nrow(tmp)])
    }
    names(cfs) <- names(cfs) <- names(cf)
    names(ses) <- names(ses) <- names(cf)
    colnames(vcv) <- rownames(vcv) <- names(cf)
    colnames(cors) <- rownames(cors) <- names(cf)
    list(coefficients=cfs, std.error=ses, vcov=vcv, cor=cors,
        B = ncol(tmp) - 1, type = attr(tmp, "type"))
}

