parListFactories <-
function(cl, type) 
{
    ## stop if rjags not found
    requireNamespace("rjags")
    clusterEvalQ(cl, requireNamespace("rjags"))
    clusterCall(cl, rjags::list.factories, type=type)
}
