@rem test.plotd.bat
@rem Stephen Milborrow Mar 2008 Durban

@echo test.plotd.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.plotd.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.plotd.Rout:
@echo.
@tail test.plotd.Rout
@echo test.plotd.R
@exit /B 1
:good1
mks.diff test.plotd.Rout test.plotd.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.plotd.save.ps
@exit /B 1
:good2
@rem test.plotd.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.plotd.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.plotd.Rout
@rm -f Rplots.ps
@exit /B  0
