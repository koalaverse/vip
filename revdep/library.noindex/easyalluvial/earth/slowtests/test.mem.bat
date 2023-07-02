@rem test.mem.bat

@echo test.mem.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.mem.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.mem.Rout:
@echo.
@tail test.mem.Rout
@echo test.mem.R
@exit /B 1
:good1
mks.diff test.mem.Rout test.mem.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.mem.save.ps
@exit /B 1
:good2
@rem test.mem.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.mem.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.mem.Rout
@rm -f Rplots.ps
@exit /B  0
