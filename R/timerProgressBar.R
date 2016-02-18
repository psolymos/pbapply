timerProgressBar <-
function(min = 0, max = 1, initial = 0)
{
    .start <- proc.time()[["elapsed"]]
    .min   <- force(min)
    .max   <- force(max)
    .i     <- force(initial)


    getVal <- function()  .i


    ## up function similar to TxtProgressBar
    up <- function(value) {
    time <- proc.time()[["elapsed"]] - .start
    .i <<- value

    i <- .i - .min
    n <- .max - .min

    if (.i > .max)
        stop("Bar is over!")
    time <- time / (i / n) - time

    leftTime <- if (i == 0)
        getTimeAsString(NULL) else getTimeAsString(time)

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

    kill <- function() invisible(NULL)

    structure(list(getVal = getVal, up = up, kill = kill), class = c("timerProgressBar","txtProgressBar"))
}

setTimerProgressBar <- setTxtProgressBar
getTimerProgressBar <- getTxtProgressBar

# converts time in seconds into ~HHh MMm SSs format
getTimeAsString <- function(time) {
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

