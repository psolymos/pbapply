errlines.default <-
function(x, y, type = "l", code = 0, width = 0, ...)
{
    if (NCOL(y) == 1 && length(y == 2) && length(x) == 1)
        y <- matrix(y, 1, 2)
    if (NCOL(y) != 2)
        stop("'y' must have 2 columns")
    type <- match.arg(substr(type, 1, 1), c("l", "b"))
    n <- length(x)
    y <- cbind(pmin(y[,1], y[,2]), pmax(y[,1],y[,2]))
    if (type == "b" && width == 0)
            type <- "l"
    if (!(code %in% 0:3))
        stop("'code' value is inappropriate")
    if (width <= 0) {
        code <- 0
    } else {
        x1 <- x - width / 2
        x2 <- x + width / 2
        for (i in 1:n) {
            if (code %in% c(1, 3))
                lines(c(x1[i], x2[i]), rep(y[i,1], 2), ...)
            if (code %in% c(2, 3))
                lines(c(x1[i], x2[i]), rep(y[i,2], 2), ...)
        }
    }
    for (i in 1:n) {
        if (type == "l") {
            lines(rep(x[i], 2), y[i,], ...)
        } else {
            lines(rep(x1[i], 2), y[i,], ...)
            lines(rep(x2[i], 2), y[i,], ...)
        }
    }
    invisible(NULL)
}
