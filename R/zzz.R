.onLoad <- function(libname, pkgname){
    if (is.null(getOption("pboptions"))) {
        type <- if (interactive())
            "timer" else "none"
        options("pboptions" = list(
            type = "timer",
            char = "+",
            txt.width = 50,
            gui.width = 300,
            style = 3,
            initial = 0,
            title = "R progress bar",
            label = ""))
    }
    invisible(NULL)
}

.onUnload <- function(libpath){
    options("pboptions"=NULL)
    invisible(NULL)
}

