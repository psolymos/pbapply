if (.Platform$OS.type != "windows") {

aspell_pkg <- 
  function(pkg,
           path = "/home/peter/svn/dcr/pkg",
           outdir = "/home/peter/svn/dcr/devel/tests",
           prog = "hunspell") 
{
    f_R <- Sys.glob(file.path(path, pkg, "R/*.R"))
    f_Rd <- Sys.glob(file.path(path, pkg, "man/*.Rd"))
    x_R <- utils:::aspell(f_R, program=prog, filter="R")
    x_Rd <- utils:::aspell(f_Rd, program=prog, filter="Rd")
    if (length(x_R[[1]]) > 0) {
        x_R$Suggestions <- sapply(x_R$Suggestions, paste, collapse=" ")
        df_R <- data.frame(Package=pkg, Subdir="R", as.data.frame(x_R))
    } else {
        df_R <- NULL
    }
    if (length(x_Rd[[1]]) > 0) {
        x_Rd$Suggestions <- sapply(x_Rd$Suggestions, paste, collapse=" ")
        df_Rd <- data.frame(Package=pkg, Subdir="Rd", as.data.frame(x_Rd))
    } else {
        df_Rd <- NULL
    }
    out <- rbind(df_R, df_Rd)
    write.csv(out, 
        file = file.path(outdir, paste("aspell_output_", pkg, ".csv", sep="")),
        row.names = FALSE)
    invisible(NULL)
}

odir <- "/home/peter/Dropbox/pkg/spellcheck"
aspell_pkg("dclone", outdir=odir)
aspell_pkg("dcmle", outdir=odir)
aspell_pkg("detect", outdir=odir)
aspell_pkg("pbapply", outdir=odir)
aspell_pkg("PVAClone", outdir=odir)
aspell_pkg("ResourceSelection", outdir=odir)
aspell_pkg("sharx", outdir=odir)
aspell_pkg("mefa", path="/home/peter/svn/mefa/pkg", outdir=odir)
aspell_pkg("mefa4", path="/home/peter/svn/mefa/pkg", outdir=odir)
aspell_pkg("vegan", path="/home/peter/svn/vegan/pkg", outdir=odir)

}

