@rem test.earthmain.gcc.bat: test 32-bit standalone earth.c with main()
@rem
@rem The gcc, Microsoft, and clang compiler batch files all test
@rem against the same reference file "test.earthmain.out.save"
@rem
@rem Stephen Milborrow Jan 2008 Durban

@echo test.earthmain.gcc.bat

@rem set the path and environment for building R packages for the 32-bit gcc compiler
@rem only do it if needed
@mks.which gcc | egrep -i "mingw32" >NUL && goto :donesetpath
@echo Modifying path for 32-bit Rtools and R
@set PATH=C:\rtools40\mingw32\bin;^
C:\rtools40\usr\bin;^
C:\Program Files\R\R-4.2.2\bin\i386;^
C:\Program Files\gs\gs9.19\bin;^
%PATH%
:donesetpath

@mks.cp "D:\bin\milbo\R400devdll\i386\R.dll" .
                                @if %errorlevel% neq 0 goto err
@mks.cp "D:\bin\milbo\R400devdll\i386\Rblas.dll" .
                                @if %errorlevel% neq 0 goto err
@mks.cp "D:\bin\milbo\R400devdll\i386\Riconv.dll" .
                                @if %errorlevel% neq 0 goto err
@mks.cp "D:\bin\milbo\R400devdll\i386\Rgraphapp.dll" .
                                @if %errorlevel% neq 0 goto err
@rem you may have to create R.lib and Rblas.lib beforehand
@mks.cp "D:\bin\milbo\R400devdll\i386\R.lib" .
                                @if %errorlevel% neq 0 goto err
@mks.cp "D:\bin\milbo\R400devdll\i386\Rblas.lib" .
                                @if %errorlevel% neq 0 goto err

gcc -DSTANDALONE -DMAIN -Wall --pedantic -Wextra -O3 -std=gnu99^
 -I"/a/r/ra/include" -I../../inst/slowtests ../../src/earth.c^
 R.lib Rblas.lib -o earthmain-gcc.exe
                                @if %errorlevel% neq 0 goto err
earthmain-gcc.exe > test.earthmain-gcc.out
                                @rem no errorlevel test, diff will do check for discrepancies
                                @rem @if %errorlevel% neq 0 goto err
@rem we use -w on mks.diff so it treats \r\n the same as \n
mks.diff -w test.earthmain-gcc.out test.earthmain.out.save
                                @if %errorlevel% neq 0 goto err

@if %errorlevel% equ 0 goto good
:err
@echo error: errorlevel %errorlevel%
@exit /B %errorlevel%
:good
@rm -f R.dll Rblas.dll Riconv.dll Riconv.dll Rgraphapp.dll R.lib Rblas.lib earthmain-gcc.* test.earthmain-gcc.* *.o
@rm -rf Release
@exit /B 0
