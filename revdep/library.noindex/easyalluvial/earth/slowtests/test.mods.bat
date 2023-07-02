@Rem test.mods.R: test earth's ability to build various models
@rem Stephen Milborrow Jan 2014 Berea

@echo test.mods.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.mods.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.mods.Rout:
@echo.
@tail test.mods.Rout
@echo test.mods.R
@exit /B 1
:good1
@echo diff test.mods.Rout test.mods.Rout.save
@rem -w to treat \n same as \r\n
@mks.diff -w test.mods.Rout test.mods.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.mods.save.ps
@exit /B 1
:good2
@rem test.mods.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.mods.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.mods.Rout
@rem @rm -f test.mods.pdf
@exit /B  0
