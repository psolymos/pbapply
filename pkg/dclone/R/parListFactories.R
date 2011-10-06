parListFactories <-
function(cl, type) 
{
    clusterEvalQ(cl, require(rjags))
    clusterCall(cl, list.factories, type=type)
}
