ticker <-
function(n)
{
    start <- proc.time()[["elapsed"]]
    n <- force(n)
    i <- (-1)
    get_time_as_string <- function(time) {
        if (is.null(time)) {
            return("~calculating")
        } else {
            if(is.infinite(time))
                return("~Inf")
        }
        sec <- round(time %% 60)
        time <- floor(time / 60)
        minutes <- floor(time %% 60)
        time <- floor(time / 60)
        hours <- time
        resTime <- ""
        if (hours > 0)
            resTime <- sprintf("%02ih ", hours)
        if (minutes > 0 || hours > 0)
            resTime <- paste(resTime, sprintf("%02im ", minutes), sep = "")
        resTime <- paste0(resTime, sprintf("%02is", sec))
        resTime
    }
    ## return value is a function w/o arguments
    function() {
    time <- proc.time()[["elapsed"]] - start
    i <<- i + 1

    if (i > n)
        stop("Bar is over!")
    time <- time / (i / n) - time

    leftTime <- if (i == 0)
        get_time_as_string(NULL) else get_time_as_string(time)

    char <- getOption("pboptions")$char
    width <- options("width")[[1]]

    minLetters <- nchar("%%%.%%% ~00h 00m 00s")
    txtWidth <- width - minLetters - 4
    text <- paste0(sprintf("%-2.2f%%", 100 * i / n), " ~", leftTime)

    if(nchar(text) < minLetters)
        text <- paste(text, paste(rep(" ",minLetters - nchar(text)), collapse = ""))
    if(txtWidth < 0 && interactive())
        cat("\r ",text)

    bb <- paste(rep(char, ceiling(txtWidth * i / n)), collapse = "")
    empty <- paste(rep(" ", floor(txtWidth * (1 - i / n))), collapse = "")

    bar <- paste("  |", bb, empty, "|", sep = "")

    if(interactive())
        cat(paste("\r", bar, text))
    } # end of return function
}
