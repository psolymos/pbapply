#dir <- "c:/svn/dcr/pkg/dclone/man/"
check_spelling <- function(dir, pattern="*.Rd", ...) {
    files <- Sys.glob(file.path(dir, pattern))
    x <- utils:::aspell(files, ...)
}

if (.Platform$OS.type != "windows") {

res1 <- check_spelling("/home/peter/svn/dcr/*/*/man")
res2 <- check_spelling("/home/peter/svn/mefa/*/*/man")
res3 <- check_spelling("/home/peter/svn/vegan/pkg/vegan/man")
## apply built-in filters "R" and "Rd", maybe "Sweave"?

}
