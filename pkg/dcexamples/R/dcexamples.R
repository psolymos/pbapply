dcexample <- 
function(topic, n.clones=NULL, ...) 
{
#    data(substitute(topic), package="dcexamples")
    nam <- deparse(substitute(topic))
    x <- get(nam, envir=.GlobalEnv)
    rval <- if (!is.null(n.clones))
        dcmle(x, n.clones=n.clones, ...) else NULL
    spacer <- paste(rep("=", nchar(nam) + 32), collapse="")
    cat("\n", spacer, "\n    Data Cloning Example: \"", 
        nam, "\"\n", spacer, "\n", sep="")
    cat("\n\n< Structure of the Data/Model Template >\n\n")
    str(x)
    cat("\n")
    if (!is.null(rval)) {
        cat("\n< Summary of the Fitted MLE Object >\n\n")
        heading <- paste("Summary of the \"", nam, "\" Example", sep="")
        getMethod("summary","dcMle")(rval, heading)
    }
    invisible(rval)
}
#dcexample(paramecium)
#dcexample(paramecium,2,n.iter=1000)

