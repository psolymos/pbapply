snowWrapper <-
function(cl, seq, fun, cldata, name="cldata", lib=NULL, size = 1, 
seed=1, kind="default", normal.kind="default", 
balancing=c("none", "load", "size"), ...)
{
    balancing <- match.arg(balancing)
    clusterSeed(cl, seed, kind, normal.kind)
    ## if object name exists in global env, make a copy as tmp, and put back in the end
    if (exists(name, envir=.GlobalEnv)) {
        assign("tmp", get("cldata", envir=.GlobalEnv))
        on.exit(rm(list=name, envir = .GlobalEnv), add=TRUE)
        on.exit(assign(name, tmp, envir = .GlobalEnv), add=TRUE)
    }
    ## loads lib on each worker
    if (!is.null(lib)) {
        for (i in lib)
            eval(parse(text=paste("clusterEvalQ(cl, library(", i, "))")))
    }
    ## place object name into global env (clusterExport can reach it)
    assign(name, cldata, envir = .GlobalEnv)
    clusterExport(cl, name)
    ## parallel work done here similarly as in parLapply
    res <- switch(balancing,
        "none" = clusterApplyLB(cl, seq, fun, ...),
        "load" = parLapply(cl, seq, fun, ...),
        "size" = parLapplySB(cl, seq, size=size, fun, ...))
    res
}
