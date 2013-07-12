::mkdir c:\svn\dcr\devel\tests\
svn export c:\svn\dcr\pkg\dclone c:\svn\dcr\devel\tests\dclone
cd c:\svn\dcr\devel\tests\
R CMD build dclone --compact-vignettes
::R CMD check dclone_*.tar.gz --as-cran
R CMD INSTALL dclone_*.tar.gz
R CMD BATCH --vanilla "c:/svn/dcr/devel/dclone_tests.R" "c:/svn/dcr/devel/tests/dclone_tests.log"


