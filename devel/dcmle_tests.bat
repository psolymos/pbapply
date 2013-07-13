::mkdir c:\svn\dcr\devel\tests\
svn export c:\svn\dcr\pkg\dcmle c:\svn\dcr\devel\tests\dcmle
cd c:\svn\dcr\devel\tests\
R CMD build dcmle --compact-vignettes
R CMD check dcmle_*.tar.gz --as-cran
R CMD INSTALL dcmle_*.tar.gz
R CMD BATCH --vanilla "c:/svn/dcr/devel/dcmle_tests.R" "c:/svn/dcr/devel/tests/dcmle_tests.log"
R CMD BATCH --vanilla "c:/svn/dcr/devel/dcexamples_tests.R" "c:/svn/dcr/devel/tests/dcexamples_tests.log"
