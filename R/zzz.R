.onLoad <- function(libname, pkgname){
    show_pb <- interactive()
    if (is.null(getOption("knitr.in.progress")))
        show_pb <- FALSE
    opts <- list(
        type = if (show_pb) "timer" else "none",
        char = "+",
        txt.width = 50,
        gui.width = 300,
        style = 3,
        initial = 0,
        title = "R progress bar",
        label = "",
        nout = 100L,
        min_time = 0)
    optsx <- getOption("pboptions")
    if (!is.null(optsx)) {
        for (i in intersect(names(opts), names(optsx)))
            opts[[i]] <- optsx[[i]]
        for (i in setdiff(names(optsx), names(opts)))
            opts[[i]] <- optsx[[i]]
    }
    options("pboptions" = opts)
    invisible(NULL)
}

.onUnload <- function(libpath){
    options("pboptions" = NULL)
    invisible(NULL)
}

