print.summary.glmexplorer <-
function(x, ...) {
    attr(x, "id") <- NULL
    class(x) <- "matrix"
    print(x)
    invisible(x)
}
