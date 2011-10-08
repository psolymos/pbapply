@echo off
echo.
echo dcmle tests
echo.
cd \svn\dcr\devel\tests
echo.
echo build source package
echo.
R CMD build "c:/svn/dcr/pkg/dcmle"
echo.
echo build binary package
echo.
R CMD build "c:/svn/dcr/pkg/dcmle" --binary
echo.
echo install source package
echo.
R CMD INSTALL dcmle_0.1-1.tar.gz
echo.
echo check package
echo.
R CMD check "c:/svn/dcr/pkg/dcmle" --outdir="c:/svn/dcr/devel/tests/"
echo.
echo run dontrun examples in dcmle
echo.
R CMD BATCH --vanilla "c:/svn/dcr/devel/dcmle_tests.R" "c:/svn/dcr/devel/tests/dcmle_tests.log"
echo.
echo done
::exit
