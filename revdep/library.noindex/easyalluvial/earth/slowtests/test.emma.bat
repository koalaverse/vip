@rem test.emma.R: regression tests for emma with plotmo
@rem Stephen Milborrow, Shrewsbury Nov 2014

@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.emma.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.emma.Rout:
@echo.
@tail test.emma.Rout
@echo test.emma.R
@exit /B 1
:good1
mks.diff test.emma.Rout test.emma.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@rem @diffps -s Rplots.ps ..\..\.#\test-reference\test.emma.save.ps
@exit /B 1
:good2
@rem test.emma.save.ps is too big to be included in the release
@rem so it is stored elsewhere
@rem diffps Rplots.ps ..\..\.#\test-reference\test.emma.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.emma.Rout
@rm -f Rplots.ps
@exit /B 0
