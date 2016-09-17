## code for terminal gif using licecap (http://www.cockos.com/licecap/)

library(pbapply)

fun <- function(x) { Sys.sleep(0.1); x^2 }
B <- 25

## default settings: bar with timer
pbo <- pboptions() # store settings
tmp <- pblapply(seq_len(B), fun)

## change type, style, and char
pboptions(type = "txt", style = 1, char = ">")
tmp <- pblapply(seq_len(B), fun)

## throbber style with timer
pboptions(type = "timer", style = 4)
tmp <- pblapply(seq_len(B), fun)

## restore default settings
pboptions(pbo)

