.onLoad <- function(libname, pkgname){
    if (is.null(getOption("pboptions"))) {
        type <- if (interactive())
            "timer" else "none"
        options("pboptions" = list(
            type = type,
            char = "+",
            txt.width = 50,
            gui.width = 300,
            style = 3,
            initial = 0,
            title = "R progress bar",
            label = "",
            nout = 100L))
    }
    invisible(NULL)
}

.onUnload <- function(libpath){
    options("pboptions"=NULL)
    invisible(NULL)
}

