load_BAM_QPAD <- function(version) {
    if (missing(version)) {
        cat("Select version of BAM QPAD estimates\n")
        cat("1 - version 20130226\n")
        cat("Enter selection: ")
        version <- readline()
    } else {
        version <- as.character(version)
    }
    while (!(version %in% c("1"))) { # available versions
        cat("Value out of range. Enter selection: ")
        version <- readline()
    }
    if (version == "1") {
        source("http://dcr.r-forge.r-project.org/qpad/BAM_QPAD_coefs_20130226.R")
        source("http://dcr.r-forge.r-project.org/qpad/BAM_QPAD_functions_20130226.R")
    }
    invisible(NULL)
}
