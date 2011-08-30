parLoadModule <-
function(cl, name, path, quiet=FALSE) 
{
    clusterEvalQ(cl, require(rjags))
    clusterCall(cl, eval, 
        load.module(name, path, quiet), env = .GlobalEnv)
}

