@rem test.ordinal.bat: ordinal models by way of package "ordinal" and earth's bx matrix
@rem Sep 2020 Petaluma

@echo test.ordinal.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.ordinal.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.ordinal.Rout:
@echo.
@tail test.ordinal.Rout
@echo test.ordinal.R
@exit /B 1
:good1
mks.diff test.ordinal.Rout test.ordinal.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.ordinal.save.ps
@exit /B 1
:good2
@rem test.ordinal.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.ordinal.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.ordinal.Rout
@rm -f Rplots.ps
@exit /B  0
