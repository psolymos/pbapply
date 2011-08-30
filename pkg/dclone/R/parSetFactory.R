parSetFactory <-
function(cl, name, type, state) 
{
    clusterEvalQ(cl, require(rjags))
    clusterCall(cl, eval, set.factory(name, type, state), env = .GlobalEnv)
}

