vcov.mcmc <- 
function(object, ...) 
{
    dclone::vcov.mcmc.list(as.mcmc.list(object), ...)
}
