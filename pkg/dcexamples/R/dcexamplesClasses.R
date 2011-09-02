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
        showfun <- function(object) {
            k <- object@n.clones
            if (is.null(k)) {
                print(summary(object@mcmc))
            } else {
                attributes(k) <- NULL
                n <- data.frame(start=object@start, end=object@end, thin=object@thin,
                    n.iter=object@end-object@start+1,
                    n.chains=object@n.chains, n.clones=k)
                digits <- max(3, getOption("digits") - 3)
                title <- paste("Object of class \"", class(object)[1L], "\"", sep="")
                cat(title, "\n\n")
                print(n, digits=digits, row.names=FALSE)
                cat("\n")
                printCoefmat(object@summary, digits = digits, signif.legend = TRUE)
                cat("\n")
                print(object@dcdiag, digits=digits, row.names=FALSE)
                cat("\n")
            }
            invisible(object)
        }
        cat("\n< Summary of the Fitted MLE Object >\n\n")
        showfun(rval)
    }
    invisible(rval)
}

#dcexample(paramecium)
#dcexample(paramecium,2,n.iter=1000)

