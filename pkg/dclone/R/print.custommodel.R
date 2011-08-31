print.custommodel <- 
function (x, deparse = FALSE, ...) 
{
    cat("Object of class \"custommodel\":\n\n")
    if (deparse) {
        cat("structure(\nc(\"", x[1], "\",\n")
        for(i in x[2:(length(x)-1)])
            cat("\"", i, "\",\n")
        cat("\"", x[length(x)], "\"),\n")
        cat("class = \"custommodel\")\n")
    } else {
        for(i in x)
            cat(i, "\n")
    }
    invisible(x)
}
