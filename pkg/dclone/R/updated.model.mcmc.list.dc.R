updated.model.mcmc.list.dc <- 
function(object, ...) 
{
    attr(attr(object, "updated.model"), "n.clones") <- nclones(object)
    attr(object, "updated.model")
}
