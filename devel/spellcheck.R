#dir <- "c:/svn/dcr/pkg/dclone/man/"
check_spelling <- function(dir, pattern="*.Rd", ...) {
    files <- Sys.glob(file.path(dir, pattern))
    x <- utils:::aspell(files, ...)
}

if (.Platform$OS.type != "windows") {

res1 <- check_spelling("c:/svn/dcr/*/*/man")
res1 <- check_spelling("c:/svn/mefa/*/*/man")
res1 <- check_spelling("c:/svn/vegan/pkg/vegan/man")

}
