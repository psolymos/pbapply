parListModules <-
function(cl) 
{
    clusterEvalQ(cl, require(rjags))
    clusterCall(cl, eval, list.modules(), env = .GlobalEnv)
}

