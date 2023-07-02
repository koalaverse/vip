@Rem test.pmethod.cv.R: example pmethod.cv model built by earth
@rem Stephen Milborrow May 2015 Berea

@echo test.pmethod.cv.bat
@"C:\PROGRA~1\R\R-4.2.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.pmethod.cv.R
@if %errorlevel% equ 0 goto good1
@echo R returned errorlevel %errorlevel%, see test.pmethod.cv.Rout:
@echo.
@tail test.pmethod.cv.Rout
@echo test.pmethod.cv.R
@exit /B 1
:good1
@echo diff test.pmethod.cv.Rout test.pmethod.cv.Rout.save
@rem -w to treat \n same as \r\n
@mks.diff -w test.pmethod.cv.Rout test.pmethod.cv.Rout.save
@if %errorlevel% equ 0 goto good2
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.pmethod.cv.save.ps
@exit /B 1
:good2
@rem test.pmethod.cv.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.pmethod.cv.save.ps
@if %errorlevel% equ 0 goto good3
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.pmethod.cv.Rout
@rm -f Rplots.ps
@exit /B  0
