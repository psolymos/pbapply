parSetFactory <-
function(cl, name, type, state) 
{
    ## stop if rjags not found
    requireNamespace("rjags")
    clusterEvalQ(cl, requireNamespace("rjags"))
    clusterCall(cl, rjags::set.factory,
        name=name, type=type, state=state)
}
