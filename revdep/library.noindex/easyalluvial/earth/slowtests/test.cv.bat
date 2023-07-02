@rem test.cv.bat: tests earth cross validation
@rem Stephen Milborrow Nov 2008 Gardens

@echo test.cv.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.cv.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.cv.Rout:
@tail test.cv.Rout
@echo test.cv.R
@echo.
@exit /B 1
:good1
mks.diff test.cv.Rout test.cv.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.cv.save.ps
@exit /B 1
@rem test.cv.save.ps is too big to be included in the release
@rem so it is stored elsewhere
:good2
diffps Rplots.ps ..\..\.#\test-reference\test.cv.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.cv.Rout
@rm -f Rplots.ps
@exit /B  0
