earth/inst/slowtests/README.txt
-------------------------------

The tests in this directory must be run manually before submitting a
new version of this package to CRAN.

They are much more comprehensive than the standard CRAN checks in
tests/tests.earth.R, but take several minutes to run.

Also they compare postscript files, and there are sometimes arbitrary
changes to the format of those postscript files due to changes in the
postscript driver across R releases.  Such changes must be manually
checked by comparing the files in a postscript viewer.  Complete
automation isn't possible.
