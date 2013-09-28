dclone.environment <- 
function(x,  n.clones = 1, multiply = NULL, unchanged = NULL, 
attrib = TRUE, ...) 
{
    dclone::dclone.list(as.list(x),  n.clones, 
        multiply, unchanged, attrib, ...)
}
