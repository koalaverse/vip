@rem makeclean.bat: clean up R package slowtests directory

@rem make sure we are in the right directory
@cd ..\..\..
                        @if %errorlevel% NEQ 0 goto err
@cd earth\inst\slowtests
                        @if %errorlevel% NEQ 0 goto err

rm -rf Debug Release .vs
rm -f ../../src/earth_res.rc ../Makedeps
rm -f *.dll *.lib *.pdb *.map *.ilk
rm -f *.ps *.pdf *.Rout *.exe *.out

@goto done
:err
@echo ==== ERROR ====
@exit /B %errorlevel%
:done
@exit /B  0
