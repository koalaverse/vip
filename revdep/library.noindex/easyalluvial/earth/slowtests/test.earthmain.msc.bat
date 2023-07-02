@rem test.earthmain.bat: test the standalone earth.c with main()
@rem
@rem The gcc, Microsoft, and clang compiler batch files all test
@rem against the same reference file "test.earthmain.out.save"
@rem
@rem Stephen Milborrow Apr 2007 Petaluma

@echo test.earthmain.msc.bat
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
@mks.cp "D:\bin\milbo\R400devdll\i386\R.lib" .
                                @if %errorlevel% neq 0 goto err
@mks.cp "D:\bin\milbo\R400devdll\i386\Rblas.lib" .
                                @if %errorlevel% neq 0 goto err

@md Debug

@rem Note: Use Microsoft VC16 (Visual Studio 2019) 32 bit.
@rem (Other versions haven't been tested and may cause spurious errors.)
@rem
@rem To set up the environment for the call to "cl" and "link" below, invoke:
@rem    C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars32.bat
@rem
@rem We use -W4 below (insteadof -W3) for lint-like warnings

cl -nologo -DSTANDALONE -DMAIN -TP -Zi -W3 -MDd -I"%ProgramFiles%\R\R-4.2.2"\src\include -I. -FpDebug\vc60.PCH -Fo"Debug/" -c ..\..\src\earth.c
                                @if %errorlevel% neq 0 goto err
@rem linker needs to be called explicitly, else we may call the wrong link program (e.g. /rtools40/usr/bin/link)
"C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Tools\MSVC\14.29.30133\bin\HostX64\x86\link.exe" -nologo -debug -out:earthmain.exe Debug\earth.obj R.lib Rblas.lib

                                @if %errorlevel% neq 0 goto err
earthmain.exe > Debug\test.earthmain.out
                                @rem no errorlevel test, diff will do check for discrepancies
                                @rem @if %errorlevel% neq 0 goto err
mks.diff Debug\test.earthmain.out test.earthmain.out.save
                                @if %errorlevel% neq 0 goto err

@rm -f R.dll Rblas.dll R.lib Rblas.lib iconv.dll Riconv.dll Rgraphapp.dll earthmain.exe *.map *.ilk *.pdb
@rm -rf Debug
@exit /B 0

:err
@exit /B %errorlevel%
