timerProgressBar <-
function(min = 0, max = 1, initial = 0, char = "=",
width = NA, title, label, style = 1, file = "", min_time = 0)
{
    if (!identical(file, "") && !(inherits(file, "connection") &&
        isOpen(file)))
        stop("'file' must be \"\" or an open connection object")
    if (max <= min)
        stop("must have 'max' > 'min'")
    if (!(style %in% 1:6))
        style <- 1
    if (style %in% c(2, 4)) # throbber only
        .counter <- force(1)

    .start <- proc.time()[["elapsed"]]
    .min   <- force(min)
    .max   <- force(max)
    .i     <- force(initial)
    .killed <- FALSE
    ## start showing pb right away when min_time = 0
    .showpb <- if (min_time > 0)
        FALSE else TRUE

    getVal <- function()  .i

    if (nchar(char, "w") < 1)
        char <- "="
    if (nchar(char, "w") > 1 && style %in% 1:4)
        char <- substr(char, 1, 1)
    if (nchar(char, "w") > 4 && style %in% 5:6)
        char <- substr(char, 1, 4)
    if (style %in% 5:6) {
        if (nchar(char, "w") == 1)
            char <- c("|", char, " ", "|")
        else if (nchar(char, "w") == 2)
            char <- c(substr(char,1,1), substr(char,2,2), " ", substr(char,1,1))
        else if (nchar(char, "w") == 3)
            char <- c(substr(char,1,1), substr(char,2,2),
                substr(char,3,3), substr(char,1,1))
        else if (nchar(char, "w") == 4)
            char <- c(substr(char,1,1), substr(char,2,2),
                substr(char,3,3), substr(char,4,4))
        if (char[2] == char[3])
            char[3] <- " "
    }
    if (is.na(width))
        width <- options("width")[[1]]

    ## |= | style progress bar with elapsed and remaining time
    up1 <- function(value) {
        if (!is.finite(value) || value < min || value > max)
            return()
        time0 <- proc.time()[["elapsed"]] - .start
        .i <<- value
        i <- .i - .min
        n <- .max - .min
        time <- time0 / (i / n) - time0

        if (.i > .min && sum(time0, time, na.rm=TRUE) > min_time)
            .showpb <<- TRUE
        if (.showpb) {
            spentTime <- paste0(" elapsed=", getTimeAsString(time0))
            leftTime <- if (i == 0)
                "" else paste0(", remaining~", getTimeAsString(time))
            minLetters <- nchar("%%%% ~00h 00m 00s", "w")

            ## 79-24=55 > 50
            txtWidth <- max(width, width - minLetters - 4)

            text <- paste0(sprintf("%-2.0f%%", 100 * i / n), spentTime, leftTime)
            if(nchar(text, "w") < minLetters)
                text <- paste(text, paste(rep(" ", minLetters - nchar(text, "w")),
                                          collapse = ""))
            if(txtWidth <= 0) {
                cat("\r ", text, file = file)
            } else {
                done <- ceiling(txtWidth * i / n)
                bb <- strrep(char, done)
                empty <- strrep(" ", txtWidth - done)
                bar <- paste(" |", bb, empty, "|", sep = "")
                cat("\r", bar, text, file = file)
            }
            flush.console()
        }
    }
    ## throbber with elapsed and remaining time
    up2 <- function(value) {
        if (!is.finite(value) || value < min || value > max)
            return()
        time0 <- proc.time()[["elapsed"]] - .start
        .i <<- value
        i <- .i - .min
        n <- .max - .min
        time <- time0 / (i / n) - time0
        if (i != 0)
            .counter <<- .counter + 1

        if (.i > .min && sum(time0, time, na.rm=TRUE) > min_time)
            .showpb <<- TRUE
        if (.showpb) {
            spentTime <- paste0(" elapsed=", getTimeAsString(time0))
            leftTime <- if (i == 0)
                "" else paste0(", remaining~", getTimeAsString(time))
            minLetters <- nchar("%%%% ~00h 00m 00s", "w")

            ## 79-24=55 > 50
            txtWidth <- max(width, width - minLetters - 4)

            text <- paste0(sprintf("%-2.0f%%", 100 * i / n), spentTime, leftTime)
            if(nchar(text, "w") < minLetters)
                text <- paste(text, paste(rep(" ", minLetters - nchar(text, "w")),
                    collapse = ""))
            bb <- strrep(char, ceiling(txtWidth * i / n))
            bar <- c("|", "/", "-", "\\")[(.counter %% 4) + 1]
            cat("\r", bar, text, file = file)
            flush.console()
        }
    }
    ## |= | style progress bar with remaining time
    up3 <- function(value) {
        if (!is.finite(value) || value < min || value > max)
            return()
        time0 <- proc.time()[["elapsed"]] - .start
        .i <<- value
        i <- .i - .min
        n <- .max - .min
        time <- time0 / (i / n) - time0

        if (.i > .min && sum(time0, time, na.rm=TRUE) > min_time)
            .showpb <<- TRUE
        if (.showpb) {
            prefix <- if (i != n)
                " ~" else " elapsed = "

            leftTime <- if (i == 0)
                getTimeAsString(NULL) else
                    if (i != n)
                        getTimeAsString(time) else getTimeAsString(time0)
            minLetters <- nchar("%%%% ~00h 00m 00s", "w")

            ## 79-24=55 > 50
            txtWidth <- max(width, width - minLetters - 4)

            text <- paste0(sprintf("%-2.0f%%", 100 * i / n), prefix, leftTime)
            if(nchar(text, "w") < minLetters)
                text <- paste(text, paste(rep(" ", minLetters - nchar(text, "w")),
                    collapse = ""))
            if(txtWidth <= 0) {
                cat("\r ", text, file = file)
            } else {
                done <- ceiling(txtWidth * i / n)
                bb <- strrep(char, done)
                empty <- strrep(" ", txtWidth - done)
                bar <- paste(" |", bb, empty, "|", sep = "")
                cat("\r", bar, text, file = file)
            }
            flush.console()
        }
    }
    ## throbber with remaining time
    up4 <- function(value) {
        if (!is.finite(value) || value < min || value > max)
            return()
        time0 <- proc.time()[["elapsed"]] - .start
        .i <<- value
        i <- .i - .min
        n <- .max - .min
        time <- time0 / (i / n) - time0

        if (i != 0)
            .counter <<- .counter + 1

        if (.i > .min && sum(time0, time, na.rm=TRUE) > min_time)
            .showpb <<- TRUE
        if (.showpb) {
            prefix <- if (i != n)
                " ~" else " elapsed = "
            leftTime <- if (i == 0)
                getTimeAsString(NULL) else
                    if (i != n)
                        getTimeAsString(time) else getTimeAsString(time0)
            minLetters <- nchar("%%%% ~00h 00m 00s", "w")

            ## 79-24=55 > 50
            txtWidth <- max(width, width - minLetters - 4)

            text <- paste0(sprintf("%-2.0f%%", 100 * i / n), prefix, leftTime)
            if(nchar(text, "w") < minLetters)
                text <- paste(text, paste(rep(" ", minLetters - nchar(text, "w")),
                    collapse = ""))
            bb <- strrep(char, ceiling(txtWidth * i / n))
            bar <- c("|", "/", "-", "\\")[(.counter %% 4) + 1]
            cat("\r", bar, text, file = file)
            flush.console()
        }
    }
    ## [=-] style progress bar with elapsed and remaining time
    up5 <- function(value) {
        if (!is.finite(value) || value < min || value > max)
            return()
        time0 <- proc.time()[["elapsed"]] - .start
        .i <<- value
        i <- .i - .min
        n <- .max - .min
        time <- time0 / (i / n) - time0

        if (.i > .min && sum(time0, time, na.rm=TRUE) > min_time)
            .showpb <<- TRUE
        if (.showpb) {
            spentTime <- paste0(" elapsed=", getTimeAsString(time0))
            leftTime <- if (i == 0)
                "" else paste0(", remaining~", getTimeAsString(time))
            minLetters <- nchar("%%%% ~00h 00m 00s", "w")

            ## 79-24=55 > 50
            txtWidth <- max(width, width - minLetters - 4)

            text <- paste0(sprintf("%-2.0f%%", 100 * i / n), spentTime, leftTime)
            if(nchar(text, "w") < minLetters)
                text <- paste(text, paste(rep(" ", minLetters - nchar(text, "w")),
                                          collapse = ""))
            if(txtWidth <= 0) {
                cat("\r ", text, file = file)
            } else {
                done <- ceiling(txtWidth * i / n)
                bb <- strrep(char[2], done)
                empty <- strrep(char[3], txtWidth - done)
                bar <- paste(" ", char[1], bb, empty, char[4], sep = "")
                cat("\r", bar, text, file = file)
            }
            flush.console()
        }
    }
    ## [=-] style progress bar with remaining time
    up6 <- function(value) {
        if (!is.finite(value) || value < min || value > max)
            return()
        time0 <- proc.time()[["elapsed"]] - .start
        .i <<- value
        i <- .i - .min
        n <- .max - .min
        time <- time0 / (i / n) - time0

        if (.i > .min && sum(time0, time, na.rm=TRUE) > min_time)
            .showpb <<- TRUE
        if (.showpb) {
            prefix <- if (i != n)
                " ~" else " elapsed = "
            leftTime <- if (i == 0)
                getTimeAsString(NULL) else
                    if (i != n)
                        getTimeAsString(time) else getTimeAsString(time0)
            minLetters <- nchar("%%%% ~00h 00m 00s", "w")

            ## 79-24=55 > 50
            txtWidth <- max(width, width - minLetters - 4)

            text <- paste0(sprintf("%-2.0f%%", 100 * i / n), prefix, leftTime)
            if(nchar(text, "w") < minLetters)
                text <- paste(text, paste(rep(" ", minLetters - nchar(text, "w")),
                    collapse = ""))
            if(txtWidth <= 0) {
                cat("\r ", text, file = file)
            } else {
                done <- ceiling(txtWidth * i / n)
                bb <- strrep(char[2], done)
                empty <- strrep(char[3], txtWidth - done)
                bar <- paste(" ", char[1], bb, empty, char[4], sep = "")
                cat("\r", bar, text, file = file)
            }
            flush.console()
        }
    }
    kill <- function() if (!.killed) {
        if (.showpb) {
            cat("\n", file = file)
            flush.console()
        }
        .killed <<- TRUE
    }
    up <- switch(style, up1, up2, up3, up4, up5, up6)
    up(initial)
    structure(list(getVal = getVal, up = up, kill = kill),
        class = c("timerProgressBar","txtProgressBar"))
}

setTimerProgressBar <- setTxtProgressBar
getTimerProgressBar <- getTxtProgressBar

## converts time in seconds into ~HHh MMm SSs format
getTimeAsString <- function(time) {
    if (length(time) > 1L)
        stop("length of input must be 1")
    if (is.null(time)) {
        return("calculating")
    } else {
        if(is.infinite(time))
            return("Inf")
    }
    sec <- round(time %% 60)
    time <- floor(time / 60)
    minutes <- floor(time %% 60)
    time <- floor(time / 60)
    days <- floor(time / 24)
    time <- floor(time %% 24)
    hours <- floor(time %% 60)
    resTime <- ""
    if (days > 0)
        resTime <- sprintf("%02id ", days)
    if (hours > 0 || days > 0)
        resTime <- paste(resTime, sprintf("%02ih ", hours), sep = "")
    if (minutes > 0 || hours > 0 || days > 0)
        resTime <- paste(resTime, sprintf("%02im ", minutes), sep = "")
    resTime <- paste0(resTime, sprintf("%02is", sec))
    resTime
}
