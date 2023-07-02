@rem test.expand.bpairs.bat

@echo test.expand.bpairs.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.expand.bpairs.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.expand.bpairs.Rout:
@echo.
@tail test.expand.bpairs.Rout
@echo test.expand.bpairs.R
@exit /B 1
:good1
mks.diff test.expand.bpairs.Rout test.expand.bpairs.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.expand.bpairs.save.ps
@exit /B 1
:good2
@rem test.expand.bpairs.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.expand.bpairs.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.expand.bpairs.Rout
@rm -f Rplots.ps
@exit /B  0
