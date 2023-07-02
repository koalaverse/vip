@rem test.earthmain.gcc64.bat: test 64 bit standalone earth.c with main()
@rem
@rem TODO I haven't yet been able to get this to work:
@rem      Crashes in daxpy_ call in FindKnot, ok with USE_BLAS = 0.

cp "C:/Program Files/R/R-4.2.2/bin/x64/R.dll" .
                                @if %errorlevel% neq 0 goto err
cp "C:/Program Files/R/R-4.2.2/bin/x64/Rblas.dll" .
                                @if %errorlevel% neq 0 goto err
cp "C:/Program Files/R/R-4.2.2/bin/x64/Riconv.dll" .
                                @if %errorlevel% neq 0 goto err
cp "C:/Program Files/R/R-4.2.2/bin/x64/Rgraphapp.dll" .
                                @if %errorlevel% neq 0 goto err

@rem you may have to create Rdll_x64.lib and Rblas_x64.lib beforehand
@cp "../../.#/Rdll_x64.lib" R.lib
                                @if %errorlevel% neq 0 goto err
@cp "../../.#/Rblas_x64.lib" Rblas.lib
                                @if %errorlevel% neq 0 goto err

@mks.which gcc | egrep -i "mingw64" >NUL && goto :donepath
@echo Modifying path for 64-bit Rtools and R
@set PATH=C:\rtools40\mingw64\bin;^
C:\rtools40\usr\bin;^
C:\Program Files\R\R-4.2.2\bin\x64;^
C:\Program Files\gs\gs9.19\bin;^
C:\Program Files (x86)\Pandoc;^
%PATH%
:donesetpath

gcc -DSTANDALONE -DMAIN -Wall --pedantic -Wextra -O3 -std=gnu99^
 -m64^
 -I"/a/r/ra/include" -I../../inst/slowtests ../../src/earth.c^
 R.lib Rblas.lib -o earthmain-gcc64.exe
                                @if %errorlevel% neq 0 goto err
@rem earthmain-gcc64.exe > test.earthmain-gcc64.out
@rem                                 @if %errorlevel% neq 0 goto err
earthmain-gcc64.exe
                                @rem no errorlevel test, diff will do check for discrepancies
                                @rem @if %errorlevel% neq 0 goto err
mks.diff test.earthmain-gcc64.out test.earthmain.out64.save
                                @if %errorlevel% neq 0 goto err

@rm -f R.dll Rblas.dll Riconv.dll Riconv.dll Rgraphapp.dll R.lib Rblas.lib earthmain-gcc.* test.earthmain-gcc64.* *.o
@exit /B 0

:err
@exit /B %errorlevel%
