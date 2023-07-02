@rem test.allowedfunc.bat
@rem Stephen Milborrow Dec 2014 Shrewsbury

@echo test.allowedfunc.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.allowedfunc.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.allowedfunc.Rout:
@echo.
@tail test.allowedfunc.Rout
@echo test.allowedfunc.R
@exit /B 1
:good1
mks.diff test.allowedfunc.Rout test.allowedfunc.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.allowedfunc.save.ps
@exit /B 1
:good2
@rem test.allowedfunc.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.allowedfunc.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.allowedfunc.Rout
@rm -f Rplots.ps
@exit /B  0
