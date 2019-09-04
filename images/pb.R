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
dt <- as.POSIXlt(Sys.Date())
yyyymm <- paste0(dt$year+1900, "-",
    if (dt$mon < 10) "0" else "", dt$mon) # mon is 0-11
x <- cran_stats(pkg)
rd <- sapply(c("Depends", "Imports", "Suggests", "LinkingTo"),
    function(z) devtools::revdep(pkg, z))
nrd <- length(unlist(rd))
p <- ggplot(x[x$start < max(x$start),], aes(end, downloads)) +
    geom_line() + geom_smooth() +
    labs(title=paste0(pkg, " (", length(nrd), " revdeps)"))
ggsave(paste0("images/pbapply-downloads-", yyyymm, ".png"), p)

## >>> prepare CRAN submission

submit_cran_template <- function(path = ".") {
    news <- readLines(file.path(path, "NEWS.md"))
    i <- which(startsWith(news, "##"))[1L:2L]
    latest <- news[i[1L]:(i[2L]-1)]
    latest <- latest[-1]
    latest <- latest[latest != ""]
    descr <- read.dcf(file.path(path, "DESCRIPTION"))
    ver <- unname(descr[1L,"Version"])
    pkg <- unname(descr[1L,"Package"])
    maint <- unname(descr[1L,"Maintainer"])
    out <- c(
        "Dear CRAN Maintainers,\n\n",
        "This is an update (version ", ver, ") of the ",
        pkg, " R extension package.\n\n",
        "The package includes the following changes:\n\n",
        paste(latest, collapse="\n"),
        "\n\nThe package tarball passed R CMD check --as-cran ",
        "without error/warning/note on Mac (current R), ",
        "Linux (old, current, devel), and Windows (current, devel R).\n\n",
        "Best wishes,\n\n",
        maint, "\npackage maintainer")
    out
}

cat(submit_cran_template(), sep="")

