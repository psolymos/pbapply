gg_traceplot <- function(x, smooth=TRUE, ...) {
    x <- stack(x)
    g <- if (smooth)
        c("line", "smooth") else "line"
    mt <- qplot(iter, value, data=x, geom=g, 
        colour=chain, ...)
    mt + facet_grid(variable~., scales = "free")
}
#gg_traceplot(regmod, main="MCMC traceplot with ggplot2")

gg_densplot <- function(x, chains=TRUE, rug=TRUE, ...) {
    x <- stack(x)
    g <- if (rug)
        c("density", "rug") else "density"
    mt <- if (chains) {
        qplot(value, data=x, geom=g,
            colour=chain, ...)
    } else {
        qplot(value, data=x, geom=g, ...)
    }
    mt + facet_grid(.~variable, scales = "free")
}
#gg_densplot(regmod, main="MCMC density plot with ggplot2")

#mt <- gg_densplot(regmod, main="MCMC density plot with ggplot2")
#mt + theme_bw()
