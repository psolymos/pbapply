## >>> code for terminal gif using licecap (http://www.cockos.com/licecap/)

library(pbapply)

fun <- function(x) { Sys.sleep(0.1); x^2 }
B <- 25

## default settings: bar with timer
pbo <- pboptions() # store settings
tmp <- pblapply(seq_len(B), fun)

## change type, style, and char
pboptions(type = "timer", style = 6, char = "[x-]")
tmp <- pblapply(seq_len(B), fun)

## change type, style, and char
pboptions(type = "tk", title="TclTk", label="Progress")
tmp <- pblapply(seq_len(B), fun)

## throbber style with timer
pboptions(type = "timer", style = 4)
tmp <- pblapply(seq_len(B), fun)

## restore default settings
pboptions(pbo)

## >>> dowload and revdep stats to save periodically

library("ggplot2")
library("dlstats")
pkg <- "pbapply"
x <- cran_stats(pkg)
rd <- devtools::revdep(pkg)
p <- ggplot(x[x$start < max(x$start),], aes(end, downloads)) +
    geom_line() + geom_smooth() +
    labs(title=paste0(pkg, " (", length(rd), " revdeps)"))
ggsave(paste0("images/pbapply-downloads-", substr(Sys.Date(), 1, 7), ".png"), p)

