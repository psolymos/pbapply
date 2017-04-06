dopb <-
function()
{
    progress.bar <- getOption("pboptions")$type
    if (!is.null(progress.bar)) {
        TYPE <- c("timer", "txt", "tk", "none")
        if (.Platform$OS.type == "windows")
            TYPE <- c(TYPE, "win")
        progress.bar <- match.arg(progress.bar, TYPE)
        if (progress.bar == "none")
            progress.bar <- NULL
        if (!is.null(getOption("knitr.in.progress")))
            progress.bar <- NULL
    }
    !is.null(progress.bar)
}
