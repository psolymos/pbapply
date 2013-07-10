:: cleanup
::rmdir c:\svn\dcr\devel\pkg_check\tests /s /q
:: export pkg dir
svn export c:\svn\dcr\pkg\ c:\svn\dcr\devel\pkg_check\tests\
svn export c:\svn\mefa\pkg\mefa c:\svn\dcr\devel\pkg_check\tests\mefa
svn export c:\svn\mefa\pkg\mefa4 c:\svn\dcr\devel\pkg_check\tests\mefa4
:: update R packages
R CMD BATCH --vanilla c:\svn\dcr\devel\pkg_check\updates.R c:\svn\dcr\devel\pkg_check\tests\updates.Rout
:: change dir to test
cd c:\svn\dcr\devel\pkg_check\tests\
:: export pkg dirs from svn
R CMD build dclone --compact-vignettes
R CMD build dcmle --compact-vignettes
R CMD build detect --compact-vignettes
R CMD build mefa --compact-vignettes
R CMD build mefa4 --compact-vignettes
R CMD build pbapply --compact-vignettes
R CMD build PVAClone --compact-vignettes
R CMD build ResourceSelection --compact-vignettes
R CMD build sharx --compact-vignettes
:: check pkgs
R CMD check dclone_*.tar.gz --as-cran
R CMD check dcmle_*.tar.gz --as-cran
R CMD check detect_*.tar.gz --as-cran
R CMD check mefa_*.tar.gz --as-cran
R CMD check mefa4_*.tar.gz --as-cran
R CMD check pbapply_*.tar.gz --as-cran
R CMD check PVAClone_*.tar.gz --as-cran
R CMD check ResourceSelection_*.tar.gz --as-cran
R CMD check sharx_*.tar.gz --as-cran


