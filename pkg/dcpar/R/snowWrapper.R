snowWrapper <-
function(cl, seq, fun, cldata, name="cldata", lib=NULL, load.balancing = TRUE, size = 1, ...)
{
    require(snow)
    ## elav args
    fun <- match.fun(fun)
    clusterfun <- if (load.balancing)
        match.fun(clusterApplyLB) else match.fun(clusterApply)
    ## if object name exists in global env, make a copy as tmp, and put back in the end
    if (exists(name, envir=.GlobalEnv)) {
        assign("tmp", get("cldata", envir=.GlobalEnv))
        on.exit(rm(list=name, envir = .GlobalEnv), add=TRUE)
        on.exit(assign(name, tmp, envir = .GlobalEnv), add=TRUE)
    }
    ## loads lib on each worker
    if (!is.null(lib))
        for (i in lib)
            eval(parse(text=paste("clusterEvalQ(cl, library(", i, "))")))
    ## place object name into global env (clusterExport can reach it)
    assign(name, cldata, envir = .GlobalEnv)
    clusterExport(cl, name)
    ## parallel work done here similarly as in clusterLapply
    s <- cluster.split(cl, seq, size)
    res <- clusterfun(cl, s, lapply, fun, ...)
    res <- docall(c, res)
    res <- res[order(unlist(s))]
    res
}
