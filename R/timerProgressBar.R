timerProgressBar <-
function(min = 0, max = 1, initial = 0, char = "=",
width = NA, title, label, style = 1, file = "")
{
    if (!identical(file, "") && !(inherits(file, "connection") &&
        isOpen(file)))
        stop("'file' must be \"\" or an open connection object")
   if (max <= min)
        stop("must have 'max' > 'min'")
#   if (!style %in% 1L:2L)
#        style <- 1

    .start <- proc.time()[["elapsed"]]
    .min   <- force(min)
    .max   <- force(max)
    .i     <- force(initial)
    .killed <- FALSE

    getVal <- function()  .i

    if (nchar(char, "w") > 1)
        char <- substr(char, 1, 1)
    if (is.na(width))
        width <- options("width")[[1]]

    up <- function(value) {
        if (!is.finite(value) || value < min || value > max)
            return()
        time <- proc.time()[["elapsed"]] - .start
        .i <<- value
        i <- .i - .min
        n <- .max - .min
        time <- time / (i / n) - time
        leftTime <- if (i == 0)
            getTimeAsString(NULL) else getTimeAsString(time)
        #minLetters <- nchar("%%%.%%% ~00h 00m 00s", "w") # 2 decimals too much
        minLetters <- nchar("%%%% ~00h 00m 00s", "w")

        ## 79-24=55 > 50
        txtWidth <- max(width, width - minLetters - 4)

        #text <- paste0(sprintf("%-2.2f%%", 100 * i / n), " ~", leftTime)
        text <- paste0(sprintf("%-2.0f%%", 100 * i / n), " ~", leftTime)
        if(nchar(text, "w") < minLetters)
            text <- paste(text, paste(rep(" ", minLetters - nchar(text, "w")),
                collapse = ""))
        if(txtWidth < 0)
            cat("\r ", text, file = file)
        bb <- paste(rep(char, ceiling(txtWidth * i / n)), collapse = "")
        empty <- paste(rep(" ", floor(txtWidth * (1 - i / n))), collapse = "")
        bar <- paste("  |", bb, empty, "|", sep = "")
        cat(paste("\r", bar, text), file = file)
        flush.console()
    }
    kill <- function() if (!.killed) {
        cat("\n", file = file)
        flush.console()
        .killed <<- TRUE
    }
#    up <- switch(style, up1, up2)
    up(initial)
    structure(list(getVal = getVal, up = up, kill = kill),
        class = c("timerProgressBar","txtProgressBar"))
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

