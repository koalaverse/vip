@rem test.offset.bat
@rem Stephen Milborrow Dec 2018 Midtown

@echo test.offset.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.offset.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.offset.Rout:
@echo.
@tail test.offset.Rout
@echo test.offset.R
@exit /B 1
:good1
mks.diff test.offset.Rout test.offset.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.offset.save.ps
@exit /B 1
:good2
@rem test.offset.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.offset.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.offset.Rout
@rm -f Rplots.ps
@exit /B  0
