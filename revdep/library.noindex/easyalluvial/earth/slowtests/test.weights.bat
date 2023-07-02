@rem test.weights.bat
@rem Stephen Milborrow Dec 2014 Shrewsbury

@echo test.weights.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.weights.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.weights.Rout:
@echo.
@tail test.weights.Rout
@echo test.weights.R
@exit /B 1
:good1
mks.diff test.weights.Rout test.weights.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.weights.save.ps
@exit /B 1
:good2
@rem test.weights.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.weights.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.weights.Rout
@rm -f Rplots.ps
@exit /B  0
