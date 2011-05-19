kdepairs.default <-
function(x, n=25, density=TRUE, contour=TRUE, ...) {
    require(MASS)
    y <- data.frame(x)
    fun.lower <- function(x1, x2, ...) {
        if (is.factor(x1)) {
            x1 <- as.integer(x1)
        }
        if (is.factor(x2)) {
            x1 <- as.integer(x2)
        }
        OK <- length(unique(x1))>2 && length(unique(x2))>2
        if (!density && !contour)
            n <- 0
        if (n>0 && OK) {
            if (density || contour)
                d <- kde2d(x1, x2, n=n)
            if (density)
                image(d, col=terrain.colors(50), add=TRUE)
            if (contour)
                graphics:::contour(d,add=TRUE)
        } else points(x1, x2)
    }
    fun.upper <- function(x1, x2, ...) {
        if (is.factor(x1)) {
            x1 <- as.integer(x1)
        }
        if (is.factor(x2)) {
            x1 <- as.integer(x2)
        }
        points(x1,x2, col="lightgrey")
        lines(lowess(x1,x2), col="darkgreen", lty=1)
        COR <- cor(x1, x2)
        text(mean(range(x1,na.rm=TRUE)), mean(range(x2,na.rm=TRUE)), 
            round(COR, 3), cex=1+COR)
    }
    panel.hist <- function(x, ...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col="gold", ...)
        box()
    }
    pairs.default(y, lower.panel=fun.lower, upper.panel=fun.upper, diag.panel=panel.hist)
    invisible(NULL)
}

