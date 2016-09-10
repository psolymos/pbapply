dopb <-
function()
{
    progress.bar <- getOption("pboptions")$type
    if (!is.null(progress.bar)) {
        progress.bar <- match.arg(progress.bar,
            c("timer", "txt", "tk", "none"))
        if (progress.bar == "none")
            progress.bar <- NULL
    }
    !is.null(progress.bar)
}

