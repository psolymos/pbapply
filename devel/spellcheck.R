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
    save(x_R, x_Rd, file=file.path(outdir, paste(pkg, "Rdata", sep=".")))
    invisible(NULL)
}

aspell_pkg("dclone")
aspell_pkg("dcmle")
aspell_pkg("detect")
aspell_pkg("pbapply")
aspell_pkg("PVAClone")
aspell_pkg("ResourceSelection")
aspell_pkg("sharx")
aspell_pkg("mefa", path="/home/peter/svn/mefa/pkg")
aspell_pkg("mefa4", path="/home/peter/svn/mefa/pkg")
aspell_pkg("vegan", path="/home/peter/svn/vegan/pkg")

}

