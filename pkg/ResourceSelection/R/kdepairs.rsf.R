kdepairs.rsf <-
function(x, n=25, density=TRUE, contour=TRUE, ...) {
    y <- data.frame(fitted=fitted(x), model.frame(x)[,-1])
    kdepairs.default(y, n, density, contour, ...)
}

