@rem test.multresp.bat
@rem Stephen Milborrow Mar 2019 Petaluma

@echo test.multresp.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.multresp.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.multresp.Rout:
@echo.
@tail test.multresp.Rout
@echo test.multresp.R
@exit /B 1
:good1
mks.diff test.multresp.Rout test.multresp.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.multresp.save.ps
@exit /B 1
:good2
@rem test.multresp.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.multresp.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.multresp.Rout
@rm -f Rplots.ps
@exit /B  0
