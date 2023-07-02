@rem test.earthc.msc.bat:
@rem
@rem This tests the earth C code.  It does this: builds test.earthc.exe
@rem (under Microsoft C VC16 (Visual Studio 2019) 32 bit, runs it,
@rem and compares results to test.earthc.out.save.
@rem
@rem You will need to tweak this file and test.earthc.msc.mak for your directories.
@rem
@rem You need to make R.lib first -- see instructions in gnuwin32/README.packages.
@rem
@rem To set up the environment for the call to "cl" and "link" in the makefile below, invoke:
@rem    C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars32.bat
@rem
@rem Stephen Milborrow Mar 2007 Forden, Wales

@echo test.earthc.msc.bat
@set CYGWIN=nodosfilewarning

@rem The following is a basic check that you have Visual Studio 2019 for 32 bit targets
@mks.which cl | egrep -i "Visual.Studio.2019.Community.VC.Tools.MSVC.*.bin.Hostx.*x86.cl" >NUL && goto donesetpath
@echo Environment is not VC16 (Visual Studio 2019) 32 bit -- please invoke vc16-32.bat
@exit /B 1
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
mks.cp "D:\bin\milbo\R400devdll\i386\R.lib" .
                                @if %errorlevel% neq 0 goto err
mks.cp "D:\bin\milbo\R400devdll\i386\Rblas.lib" .
                                @if %errorlevel% neq 0 goto err

@rem @md Release
@rem @nmake -nologo CFG=Release -f test.earthc.msc.mak

@rem The advantage of using Debug is that memory leaks are reported.
@rem It is much slower though.
@md Debug
@nmake -nologo CFG=Debug -f test.earthc.msc.mak

@if %errorlevel% equ 0 goto good
@echo error: errorlevel %errorlevel%
@exit /B %errorlevel%
:good
@rm -f R.dll Rblas.dll R.lib Rblas.lib iconv.dll Riconv.dll Rgraphapp.dll
@rm -f test.earthc.main.exe test.earthc.main.map test.earthc.main.ilk *.pdb
@rm -rf Debug
@rm -rf Release
@exit /B 0
