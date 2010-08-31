dctutor <-
function(tutorial = NULL, docs = TRUE, src = TRUE, demo = FALSE)
{
    if (is.null(tutorial)) {
        paths <- .find.package(package="dctutor", lib.loc=NULL, verbose = getOption("verbose"))
        paths <- paths[file_test("-d", file.path(paths, "demo"))]
        db <- matrix(character(0L), nrow = 0L, ncol = 4L)
        for (path in paths) {
            entries <- NULL
            if (file_test("-f", INDEX <- file.path(path, "Meta", "demo.rds")))
                entries <- .readRDS(INDEX)
            if (NROW(entries))
                db <- rbind(db, cbind(basename(path), dirname(path), entries))
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")
        footer <- NULL
        y <- list(title = "Tutorials", header = NULL, results = db, 
            footer = footer)
        class(y) <- "packageIQR"
        return(y)
    } else {
        if (docs)
            dcdocs(tutorial)
        if (src)
            dcdocs(tutorial, ext = ".R")
        if (demo)
            demo(tutorial, character.only=TRUE)
    }
}
