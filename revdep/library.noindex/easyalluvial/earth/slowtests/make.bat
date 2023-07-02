@rem earth/inst/slowtests/make.bat

@call test.earthmain.gcc.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.earthmain.clang.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.earthmain.msc.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.earthc.msc.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.earthc.gcc.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.earthc.msc.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.earthc.clang.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.numstab.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.mods.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.incorrect.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.big.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.weights.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.expand.bpairs.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.bpairs.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.full.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.glm.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.allowedfunc.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.cv.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.pmethod.cv.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.varmod.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.varmod.mgcv.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.plotd.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.offset.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.ordinal.bat
                        @if %errorlevel% NEQ 0 goto err
@call test.multresp.bat
                        @if %errorlevel% NEQ 0 goto err

@rem TODO R 4.2.0: Removed following because "Warning: package 'emma' is not available for this version of R"
@rem @call test.emma.bat
@rem                         @if %errorlevel% NEQ 0 goto err

@rem TODO With some versions of R, test.mem gives different results per run (first seen Sep 2020, R 4.0.3)
@rem @call test.mem.bat
                        @if %errorlevel% NEQ 0 goto err

@goto done
:err
@echo ==== ERROR ====
@exit /B %errorlevel%
:done
@rm -f ../../src/earth_res.rc ../Makedeps
@rm -f test.*.pdf *.dll *.lib *.pdb
@exit /B  0
