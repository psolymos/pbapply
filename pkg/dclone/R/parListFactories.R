parListFactories <-
function(cl, type) 
{
    clusterEvalQ(cl, require(rjags))
    clusterCall(cl, eval, list.factories(type), env = .GlobalEnv)
}

