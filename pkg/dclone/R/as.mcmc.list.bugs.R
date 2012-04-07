as.mcmc.list.bugs <-
function(x, ...)
{
    ## retrieve coda samples
    sarr <- x$sims.array
    ## rearranging the array into coda mcmc.list format
    res <- lapply(1:x$n.chains, function(i) sarr[,i,, drop=FALSE])
    DIM <- dim(res[[1]])[-2]
    DIMNAMES <- dimnames(res[[1]])[-2]
    for (i in 1:x$n.chains) {
        dim(res[[i]]) <- DIM
        dimnames(res[[i]]) <- DIMNAMES
    }
    ## retrieve ts attributes
    niter <- NROW(res[[1]])
    start <- x$n.burnin+1
    end <- x$n.iter
    thin <- x$n.thin
#    nobs <- (end - start + thin) / thin
    nobs <- floor((end - start) / thin + 1)
    ## some tweaking for OpenBUGS
    if (niter < nobs) {
        start <- start + thin - 1
    }
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
