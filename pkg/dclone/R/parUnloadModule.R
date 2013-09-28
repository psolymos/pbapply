parUnloadModule <-
function(cl, name, quiet = FALSE) 
{
    ## stop if rjags not found
    requireNamespace("rjags")
    clusterEvalQ(cl, requireNamespace("rjags"))
    clusterCall(cl, rjags::unload.module,
        name=name, quiet=quiet)
}
