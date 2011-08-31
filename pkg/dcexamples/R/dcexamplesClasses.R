dcexample <- 
function(topic, n.clones=NULL, ...) 
{
#    data(topic)
#    x <- get(topic)
    cat("dcexample: \"", deparse(substitute(topic)), "\"\n\n")
    cat("..@data\n")
    print(str(topic@data))
    cat("..@model\n")
    print(custommodel(topic@model))
    if (!is.null(n.clones))
        dcmle(topic, n.clones, ...) else invisible(NULL)
}
dcexample(topic)
