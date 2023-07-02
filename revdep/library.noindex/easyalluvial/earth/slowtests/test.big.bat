@rem test.big.bat: This tests earth on a biggish model
@rem This is the test mentioned in the earth man page "Big Models" section
@rem Stephen Milborrow Mar 2008 Durban

@echo test.big.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.big.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.big.Rout:
@echo.
@tail test.big.Rout
@echo test.big.R
@exit /B 1
:good1
@echo diff test.big.Rout test.big.Rout.save
@mks.diff test.big.Rout test.big.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.big.save.ps
@exit /B 1
:good2
@rem test.big.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.big.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.big.Rout
@rm -f Rplots.ps
@exit /B  0
