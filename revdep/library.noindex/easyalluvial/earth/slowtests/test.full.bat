@rem test.full.bat: this does a regression test of earth
@rem Stephen Milborrow Apr 2007 Petaluma

@echo test.full.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.full.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.full.Rout:
@echo.
@tail test.full.Rout
@echo test.full.R
@exit /B 1
:good1
mks.diff test.full.Rout test.full.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.full.save.ps
@exit /B 1
:good2
@rem test.full.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.full.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.full.Rout
@rm -f Rplots.ps
@exit /B  0
