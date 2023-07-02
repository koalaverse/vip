@rem test.bpairs.bat

@echo test.bpairs.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.bpairs.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.bpairs.Rout:
@echo.
@tail test.bpairs.Rout
@echo test.bpairs.R
@exit /B 1
:good1
mks.diff test.bpairs.Rout test.bpairs.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.bpairs.save.ps
@exit /B 1
:good2
@rem test.bpairs.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.bpairs.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.bpairs.Rout
@rm -f Rplots.ps
@exit /B  0
