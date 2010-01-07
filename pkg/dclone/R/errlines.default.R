errlines.default <-
function (x, y, type = "l", code = 0, width = 0, vertical = TRUE, col=1, bg = NA, ...)
{
    if (NCOL(y) == 1 && length(y == 2) && length(x) == 1) 
        y <- matrix(y, 1, 2)
    if (NCOL(y) != 2) 
        stop("'y' must have 2 columns")
    type <- match.arg(substr(type, 1, 1), c("l", "b"))
    n <- length(x)
    y <- cbind(pmin(y[, 1], y[, 2]), pmax(y[, 1], y[, 2]))
    if (type == "b" && width == 0) 
        type <- "l"
    if (!(code %in% 0:3)) 
        stop("'code' value is inappropriate")
    x1 <- x - width/2
    x2 <- x + width/2
    bg <- rep(bg, n)[1:n]
    col <- rep(col, n)[1:n]
    for (i in 1:n) {
        if (type == "b" && !is.na(bg)) {
            if (vertical) {
                polygon(c(x1[i], x1[i], x2[i], x2[i]), c(y[i,], rev(y[i,])), col=bg[i])
            } else {
                polygon(c(y[i,], rev(y[i,])), c(x1[i], x1[i], x2[i], x2[i]), col=bg[i])
            }
        }
    }
    if (width <= 0) {
        code <- 0
    }
    else {
        for (i in 1:n) {
            if (code %in% c(1, 3)) 
                if (vertical) {
                  lines(c(x1[i], x2[i]), rep(y[i, 1], 2), col=col[i], ...)
                }
                else {
                  lines(rep(y[i, 1], 2), c(x1[i], x2[i]), col=col[i], ...)
                }
            if (code %in% c(2, 3)) 
                if (vertical) {
                  lines(c(x1[i], x2[i]), rep(y[i, 2], 2), col=col[i], ...)
                }
                else {
                  lines(rep(y[i, 2], 2), c(x1[i], x2[i]), col=col[i], ...)
                }
        }
    }
    for (i in 1:n) {
        if (type == "l") {
            if (vertical) {
                lines(rep(x[i], 2), y[i, ], col=col[i], ...)
            }
            else {
                lines(y[i, ], rep(x[i], 2), col=col[i], ...)
            }
        }
        else {
            if (vertical) {
                lines(rep(x1[i], 2), y[i, ], col=col[i], ...)
                lines(rep(x2[i], 2), y[i, ], col=col[i], ...)
            }
            else {
                lines(y[i, ], rep(x1[i], 2), col=col[i], ...)
                lines(y[i, ], rep(x2[i], 2), col=col[i], ...)
            }
        }
    }
    invisible(NULL)
}
