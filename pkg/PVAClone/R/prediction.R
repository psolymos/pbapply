## how to predict missing value
predict.missing <- 
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
    f <- jags.fit(dcd@data, 
        params="x", 
        model=dcd@model, ...)
#    f <- dcmle(dcd, 
#        params=paste(node, "[",i,",1]",sep=""), 
#        n.clones=1, ...)
#    pred <- as.matrix(f@mcmc)
    pred <- as.matrix(f)
    colnames(pred) <- paste("value", i, sep="_")
    if (obs.error != "poisson")
        pred <- exp(pred)
    attr(pred, "index") <- i
    pred
}
## how to predict latent log abundances
predict.latent <- 
function(x, ...)
{
    if (!inherits(x, "pva"))
        stop("'x' must be of class 'pva'")
    if (x@model@obs.error == "none") {
        if (any(is.na(x@observations)))
            stop("no latent variable in model,",
                " try 'predict.missing' for missing values")
        stop("no latent variable in model")
    }
    dcd <- x@dcdata
    dcd@model <- x@model@predmodel
    f <- jags.fit(dcd@data, 
        params="x", 
        model=dcd@model, ...)
#    f <- dcmle(dcd, 
#        params="x", 
#        n.clones=1, ...)
#    pred <- as.matrix(f@mcmc)
    pred <- as.matrix(f)
    colnames(pred) <- paste("value", 1:ncol(pred), sep="_")
    pred
}

