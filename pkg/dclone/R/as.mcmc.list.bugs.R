as.mcmc.list.bugs <-
function(x, ...)
{
    ## retrieve coda samples
    sarr <- x$sims.array
    ## exclude deviance monitor
    sarr <- sarr[,,dimnames(sarr)[[3]] != "deviance", drop=FALSE]
    ## rearranging the array into coda mcmc.list format
    res <- lapply(1:x$n.chains, function(i) sarr[,i,])
    ## retrieve ts attributes
    niter <- nrow(res[[1]])
    start <- x$n.burnin+1
    end <- x$n.iter
    thin <- x$n.thin
    nobs <- floor((end - start)/thin + 1)
    ## NOTE: thin != 1 values can cause problems
    ## error message produced if this is the case
    if (niter < nobs) 
        stop("can't coerce 'bugs' object as 'mcmc.list',\nconsider refitting the model with 'n.thin = 1'")
    ## makes mcmc objects
    res <- lapply(res, function(z) mcmc(data = z,
        start = start, end = end, thin = thin))
    ## coercing into mcmc.list
    res <- as.mcmc.list(res)
    ## retrieves n.clones attr
    n.clones <- attr(x, "n.clones")
    ## final class determination based on n.clones
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}
