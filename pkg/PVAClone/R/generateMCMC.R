## how to generate missing values
generateMissing <- 
function(x, ...)
{
    if (!inherits(x, "pva"))
        stop("'x' must be of class 'pva'")
    i <- which(is.na(x@observations))
    if (!length(i))
        stop("no missing observations")
    obs.error <- x@model@obs.error
    node <- switch(obs.error,
        "none" = "x",
        "normal" = "y",
        "poisson" = "O")
    dcd <- x@dcdata
    dcd@model <- x@model@predmodel
    Params <- paste(node, "[",i,",1]",sep="")
    f <- jags.fit(dcd@data, 
        params=Params,
        model=dcd@model, ...)
    pred <- as.matrix(f)
    if (length(i) > 1)
        pred <- pred[,match(Params, colnames(pred))]
    colnames(pred) <- paste("value", i, sep="_")
    if (obs.error != "poisson")
        pred <- exp(pred)
    attr(pred, "index") <- i
    pred
}
## how to generate latent log abundances
generateLatent <- 
function(x, ...)
{
    if (!inherits(x, "pva"))
        stop("'x' must be of class 'pva'")
    if (!any(is.na(x@observations)) && x@model@obs.error == "none") {
        warning("no latent variable in model")
        pred <- matrix(log(x@observations), nrow=1)
    } else {
        if (x@model@obs.error == "none")
            warning("no latent variable in model")
        dcd <- x@dcdata
        dcd@model <- x@model@predmodel
        f <- jags.fit(dcd@data, 
            params="x", 
            model=dcd@model, ...)
        pred <- as.matrix(f)
    }
    colnames(pred) <- paste("value", 1:ncol(pred), sep="_")
    pred
}
