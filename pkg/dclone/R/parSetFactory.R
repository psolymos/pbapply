parSetFactory <-
function(cl, name, type, state) 
{
    clusterEvalQ(cl, require(rjags))
    clusterCall(cl, set.factory,
        name=name, type=type, state=state)
}
