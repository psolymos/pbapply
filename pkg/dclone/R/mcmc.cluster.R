mcmc.cluster <-
function(cl, seq, fun, cldata, name="cldata", lib=NULL, load.balancing = TRUE, ...)
{
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
        eval(parse(text=paste("clusterEvalQ(cl, library(", lib, "))")))
    ## place object name into global env (clusterExport can reach it)
    assign(name, cldata, envir = .GlobalEnv)
    clusterExport(cl, name)
    ## parallel work done here and return
    res <- clusterfun(cl, seq, fun, ...)
    res
}
