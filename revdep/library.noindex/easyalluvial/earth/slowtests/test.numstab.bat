@rem test.numstab.bat:

@echo test.numstab.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.numstab.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.numstab.Rout:
@echo.
@tail test.numstab.Rout
@echo test.numstab.R
@exit /B 1
:good1
mks.diff test.numstab.Rout test.numstab.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.numstab.save.ps
@exit /B 1
:good2
@rem test.numstab.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.numstab.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.numstab.Rout
@rm -f Rplots.ps
@exit /B  0
