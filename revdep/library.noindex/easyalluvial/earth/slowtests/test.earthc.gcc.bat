@rem test.earthc.gcc.bat:
@rem
@rem This tests the earth C code.  It does this: builds test.earthc.exe
@rem (under gcc), runs it, and compares results to test.earthc.out.save.
@rem
@rem You will need to tweak this file and test.earthc.gcc.mak for your directories.
@rem
@rem You need to make R.lib first -- see instructions in gnuwin32/README.packages.

@echo test.earthc.gcc.bat

@set CYGWIN=nodosfilewarning

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
gcc -DSTANDALONE -m32 -O3 -std=gnu99^
 -I"/a/r/ra/include" -I../../inst/slowtests^
 --pedantic -Wall -Wextra -Wno-unused-parameter -Wno-unused-variable^
 ../../src/earth.c test.earthc.c^
 R.lib Rblas.lib -o test.earthc.exe
                                @if %errorlevel% neq 0 goto err
test.earthc.exe >test.earthc.out
                                @if %errorlevel% neq 0 goto err
@rem we use -w on mks.diff so it treats \r\n the same as \n
diff -w test.earthc.out test.earthc.out.save
                                @if %errorlevel% neq 0 goto err
@if %errorlevel% equ 0 goto good
:err
@echo error: errorlevel %errorlevel%
@exit /B %errorlevel%
:good
@rm -f R.dll Rblas.dll R.lib Rblas.lib iconv.dll Riconv.dll Rgraphapp.dll
@rm -f test.earthc.exe test.earthc.map test.earthc.main.ilk test.earthc.out *.pdb
@exit /B 0
