parUnloadModule <-
function(cl, name, quiet=FALSE) 
{
    clusterEvalQ(cl, require(rjags))
    clusterCall(cl, eval, 
        unload.module(name, quiet), env = .GlobalEnv)
}

