@rem test.varmod.mgcv.bat
@rem mgcv has to be tested separately because of clashes between library(gam) and library(mgcv)
@rem Stephen Milborrow Apr 2015 Berea

@echo test.varmod.mgcv.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.varmod.mgcv.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.varmod.mgcv.Rout:
@echo.
@tail test.varmod.mgcv.Rout
@echo test.varmod.mgcv.R
@exit /B 1
:good1
mks.diff test.varmod.mgcv.Rout test.varmod.mgcv.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.varmod.mgcv.save.ps
@exit /B 1
:good2
@rem test.varmod.mgcv.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.varmod.mgcv.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.varmod.mgcv.Rout
@rm -f Rplots.ps
@exit /B  0
