parListModules <-
function(cl) 
{
    ## stop if rjags not found
    requireNamespace("rjags")
    clusterEvalQ(cl, requireNamespace("rjags"))
    clusterEvalQ(cl, rjags::list.modules())
}
