@rem test.glm.bat

@echo test.glm.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.glm.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.glm.Rout:
@echo.
@tail test.glm.Rout
@echo test.glm.R
@exit /B 1
:good1
mks.diff test.glm.Rout test.glm.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.glm.save.ps
@exit /B 1
:good2
@rem test.glm.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.glm.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.glm.Rout
@rm -f Rplots.ps
@exit /B  0
