# Changes to the earth package

## 5.3.2  Jan 26, 2023

    Modified use of "bool" in the C code to conform to C23 (which predefines "bool").

    Changed sprintf to snprintf to prevent a warning in the Debian build for CRAN.

    Changed the "NEWS" filename to "NEWS.md", and changed it to markdown format.

    Earth now issues an error if colnames in x are duplicated because of factor expansion.
    Thanks to Jens Heumann at Jacobs Center UZH for spotting this.

## 5.3.1  July 19, 2021

    Minor updates for R version 4.1.0:

    o Updates to the test scripts for ordered factors.

    o Update to the code that reports missing arguments.

## 5.3.0  Oct 10, 2020

    Fixed a minor bug where occasionally, for binary variables,
    earth put a 1 when it should have put a 2 into the dirs matrix.
    The bug was invisible unless you accessed earth's dirs matrix directly.
    In some circumstances this fix may cause earth to build slightly different
    models (an internal boundary check for almost-zero in earth.c was modified).
    Thanks to Max Kuhn for reporting this issue.

    Updated the .c and .h files to use the more modern header
    file <stdbool.h> where appropriate (i.e. when not in an R environment).
    We now also check that sizeof(bool)==sizeof(Rboolean) when called from R.

    Updated some trace prints in earth.c (only affects very high trace levels).

    Test suite additions: Added test.earthc.gcc.bat and
    test.earthc.clang.bat. Extended inst/slowtests/test.earthc.c to
    include binary predictors. Added test.numstab.R to test models which
    are known to give (slightly) different results with different
    compilers. Extended the clang tests. We now use version VC16 (Visual
    Studio 2019) of the Microsoft compiler. Miscellaneous other test updates.

    Minor documentation updates.

## 5.2.0 Sep 15, 2020

    Earth now has better support for models with unusual variable names:
    for example, variable names with spaces in them, and formula terms
    like "as.numeric(x1)".

    Earth model selection graph:

      * The text "selected model" is now first in the menu.
      * Minor adjustments for vertical lines which overlay each other.
      * The graph with nprune or pmethod="cv" models is now a little clearer.

    Better support for residual plots for earth-glm models.

    By default, plotmo now plots the offset if specified in the
    earth model formula.

    Under certain circumstances when nprune was used with pmethod="cv",
    summary.earth would issue an incorrect error message.
    Fixed that with help from Boris Leroy at the
    Museum national d'Histoire naturelle (Paris).

    Earth objects now have a "modvars" field. See the help page for earth.object.
    Earth objects no longer have a "namesx.org" field, and the "namesx" field
    is deprecated. These fields have been subsumed by the "modvars" field.

    Updated the libraries shared with the earth and plotmo packages.
    Support for unusual variable names (mentioned above) required a
    fairly substantial change to the internal handling of formulas. It
    also affected the code that selects which variables to plot in plotmo.
    For easier maintenance, the source file earth.R was split into earth.R,
    earth.fit.R, and earth.prune.R.  The new file naken.R was added for
    handling model variables.

    Extended the test scripts and updated them for R version 4.0.2.
    Added new test files test.emma.R and test.ordinal.R.

    Some documentation updates.

## 5.1.2 Nov 4, 2019

    Changed the type of the "lindeps" argument of the Fortran subroutine
    "SING" from LOGICAL to INTEGER, to satisfy a CRAN check.

    Improved handling of linear dependencies in bx (before the backward pass),
    although it is unlikely that such dependencies would exist in any earth model.

    Improved error reporting in leaps routines, although it is unlikely that
    such errors could occur in earth.

    Updated test scripts for the new random number generator
    that came with R version 3.6.0.

## 5.1.1 Apr 11, 2019

    Earth now runs with R versions 3.4.0 or higher (whereas before
    the R version had to be 3.5.0 or higher).  This restriction was lifted
    after running the tests in earth/inst/slowtests on R version 3.4.0.

    Minor updates to libraries shared with plotmo and rpart.plot.

## 5.1.0 Apr 3, 2019

    Fixed a formatting bug in the trace output for cross validation.
    Thanks to Dirk Eddelbuettel.

    Fixed small memory leaks that sometimes occurred in the C code if error()
    was invoked or the user hit ^C.  Changed the way memory is protected
    in allowed.c to pacify CRAN rchk.  Thanks to Tomas Kalibera.
    Changed the name of the fallback memory release function to FreeEarth
    (it was FreeR) and made it suitable for use in environments other than R.

    If nfold > 1 and keepxy = 2, we now use keepxy=TRUE on the fold models. This
    allows plot.earth and plotmo on the fold models e.g. plot(mod$cv.list[[3]]).
    Also updated plotmo to remind user of this.

    Tweaked the trace message that is issued when glm doesn't converge.

    Documentation and test code updates.

## 5.0.0 March 20, 2019

    Two-column binomial pair responses are now handled in a way that is more
    consistent with glm() i.e. regression on the fraction_true, weighted by
    the response rowsums.  We now support cross-validation of binomial
    pair models.  The function expand.bpairs was added.  We no longer
    supports multiple-response binomial-pair models, and the bpairs
    argument is thus no longer necessary (i.e. we support a maximum of
    one binomial-pair response; the fact that the response is a
    binomial pair is automatically detected when family=binomial).

    We now save some additional GLM stats with earth-glm models. Some error
    message have been improved.  For earth-glm models coef.earth() now returns
    the earth-glm coefficients (for these models, use coef.earth(mod,
    type="earth") to get the internal earth model coefficients).

    We now support "+" on the left side of formulas to specify multiple responses.
    Scale.y now works as documented for multiple response models.

    Fixed a bug where predict.earth issued an incorrect error message under
    certain conditions when a factor was passed as a string in the newdata.
    (This was the same as the old predict.lm "xlevels" bug.)
    Thanks to Meleksen Akin for help here.

    Fixed a bug where sometimes logical variables weren't displayed by plotmo if
    there were also factors in the model (you had to use all1=TRUE in plotmo).

    The model plot now avoids displaying ".5" in the Number Of Terms axis.

    Changes to some prints (mostly whitespace), for clarity.
    Updates to the documentation and shared libraries.

## 4.7.0 Jan 2, 2019

    Earth no longer ignores the offset term in formulas.
    Thanks to Dag Johan Steinskog for help on this.

    The names of factor arguments are now handled slightly differently in
    the x argument for earth.default. This prevents a problem where plotmo
    mistakenly mistook certain models for an intercept-only model.  (The
    models in question are those built with earth.default with a factor
    as the only column in the x matrix.)

    The type arguments for residuals.earth and fitted.earth were tweaked.
    For earth-glm models, plot.earth now plots the "response" residuals.
    For the behaviour of previous versions with earth-glm models, pass
    type="earth" to plot.earth to plot the earth (linear) residuals.

    Added some extra information to summary(glm.model).
    It now prints the AIC and the deviance ratio (defined analogously
    to RSquared as 1 - deviance / null.deviance).

    The print.earth function now includes summary of the weights and
    offsets, if any.

    For multiple-response models with weights, the weights are now
    accounted for when calculating the per-response GRSq (and not
    just for the overall GRSq).

    With plotmo(earth.model, all1=TRUE) now really plots all variables,
    (including variables that aren't used in the model).

    Added "LazyData: yes" to the DESCRIPTION file.

    Touch ups to documentation, code, and test scripts.  Minor changes
    to prevent warnings when options(warnPartialMatchArgs=TRUE).

## 4.6.3 May 7, 2018

    Earth's "allowed" argument was ignored when used with R version 3.5.0
    (because of the byte compiler in that new version of R). Fixed that by
    internally in earth invoking .Call("ForwardPassR") instead of .C("ForwardPassR").

    The vignettes are now compressed with gs and qpdf as in tools::compactPDF,
    (but that happens outside the standard CRAN build system).  It does mean
    that the tar.gz file for earth is a little smaller (now 1342 kByte).

    Made some internal changes to earth.c to quieten minor clang warnings.

## 4.6.2 Mar 20, 2018

    Minor documentation updates for the Auto.linpreds argument.

## 4.6.1 Mar 1, 2018

    Added support for package gam version 1.15 and higher (the S3 class of
    gam objects changed from "gam" to "Gam" to prevent clashes with the
    mgcv package).  Earth variance models now work with both the old and
    new versions of gam.

## 4.6.0 Dec 14, 2017

    Added the Auto.linpreds argument. See the earth help page for details.
    Thanks to Ceyda Yazici for help on this.

    Further extensions to argument handling with pmethod="cv"
    (necessary when formula is a local variable in function calling earth.formula).

## 4.5.1  Jul 26, 2017

    With pmethod="cv", you can now use arguments like "thresh" when earth
    is called in a function for which "thresh" is a local variable.
    Thanks to Matthew Watkins for help on this.

## 4.5.0  Apr 20, 2017

    Removed trailing NULL entries in R_CMethodDef and R_FortranMethodDef to
    match changes to Rdynload.c in R core source code above version 3.3.3.

## 4.4.9  Feb 19, 2017

    Fixed missing terminal entry in rentries.c:fortranEntries.
    Minor tweaks to C and Fortran files for clang and vcc warnings.

## 4.4.8  Feb 18, 2017

    Minor updates to model.matrix.earth:
      Fixed a problem which can occur when the new x is a vector.
      We return NA if variables are missing in newdata.
      Extended the tests for model.matrix.earth in the slowtests directory.

    We now call R_init_earth (to quieten CRAN check complaints).
    To do that, added the file rentries.c and made changes to ForwardPassR.

    Minor documentation updates.  Added DOI references to Friedman's papers
    in the help pages as requested by Uwe Ligges.

## 4.4.7  Oct 18, 2016

    Updates to the documentation and to the libraries
    shared with plotmo and rpart.plot.

## 4.4.6  Sep 5, 2016

    Added ... to argument list for all plotmo.* functions
    for compatibility with plotmo version 3.2.0 and higher.

    Tweaked leaps.f to eliminate gfortran warnings about the
    deprecated way that DO loops were terminated.

## 4.4.5  Aug 19, 2016

    Fixed issue in earth.c with the latest gcc which calculates a
    negative almost-zero GRSq (something like -1e-17).
    This messed up the test scripts because it printed as -0.0000.

    Merged the library source file lib.R with the plotmo and
    rpart.plot packages's lib.R.

    Updated cross-validation section in vignette.

## 4.4.4  Jan 08, 2016

    Fixed error message when predicting with a weighted model with a factor variable.
    Thanks to Damien Georges.

    Added a few type casts needed for clang and for matrices bigger than 2GB.
    Thanks to Todd Rudick.

    Removed a leftover debug call to cat("gc\n").
    Thanks to Dirk Eddelbuettel.

    Updated some stale web links in the documentation.

## 4.4.3  Sep 26, 2015

    Minor changes to satisfy CRAN checks.

    The print_summary function now has special handling for lists.

    The stopifnot.identifier function now allows ":" in identifier names.

## 4.4.2  Jun 24, 2015

    In model.matrix.earth we no longer unnecessarily issue the message:
       variable 'foo' was fitted with type "nmatrix.1"
       but type "numeric" was supplied

## 4.4.1  Jun 17, 2015

    Fixed an issue with earth's interaction with caret in certain
    situations. Thanks to Alexios Ghalanos for help on this.

    Documentation updates.

## 4.4.0  May 29, 2015

    Changes to earth.c. The following changes mean that earth will
    produce slightly different models from previous releases:

        o For minspan counting in interaction terms, we now only count cases
          that are supported by the parent term (i.e., cases that are in the zero
          part of the parent hinge are not counted).  This makes earth more
          robust against overfitting on the edges of the training predictor space.
          In previous versions, earth could create a hinge at the edge of the
          predictor space, then crank up the hinge coefficient to a huge
          value because of one or two outlying cases. This is now less likely.
          Thanks to Louise Corron at Universite Aix-Marseille for help
          characterizing this.

        o Modified the code for weighted models, mostly for consistency with
          non-weighted models.

        o Changed a numerical tolerance in FindKnot for better support of
          "sawtooth" responses (responses which sharply zigzag up and down as
          the predictor increases).  The numerical tolerance was changed because
          we were seeing a breakdown caused by the the strategy of "start at one
          end of the predictor range and search downwards for knots".  The
          asymmetrical nature of this search tends to honor potential knots at
          the high end of the predictor range at the expense of knots at the
          low end before numerical tolerances kick in.  Thanks to Greg Jensen
          at Columbia for help on this.

        o Other minor updates, particularly to trace prints. Values that
          are almost zero are now printed as zero, for more consistency
          in trace prints across different architectures.

    The weights argument has now been comprehensively tested and the
    warning "support of weights is provisional" is no longer issued.

    Earth now supports pmethod = "cv", which uses cross-validation to
    select the optimum number of model terms.
    Thanks to the sabermetrician Jonathan Judge for help on this.

    Earth now supports bigger models with less memory thrashing.  It calls
    gc() internally where necessary.  Leverages are calculated only for
    non-big models (since memory use peaks when calculating leverages).

    The model selection plot has had various enhancements.

    Various other minor code and documentation updates.

## 4.3.0  Apr 30, 2015

    We now support data matrices bigger than 2GB.

    The plot.earth function now invokes plotres in the plotmo package.
    Reparameterized the argument list of plot.earth, but maintained
    back-compatibility by using the "dots" routines.

    Numerous minor code clean-ups.

## 4.2.0  Jan 12, 2015

    plot.earth:
        o Changed the default color scheme to black and red (was black and lightblue)
          for consistency with plot.lm and similar functions, and for readability.
          A result of this change is that col.line is now once again called col.rsq.
        o The type="pearson" arg is now type="student", since the stddev
          estimated by the varmod now includes a leverage correction.
        o The versus arg now takes numeric values, and can be used to plot
          the residuals versus the response or the leverages.
        o Added "abs" to displayed spearman correlation text to minimize confusion.
        o The error handling for the versus argument is improved.

    minspan and endspan:
        o The positioning of knots is now more symmetrical (in that we now have
          an equal number of skipped cases at each end of the predictor range).
        o We always allow at least one knot, even if endspan and minspan are such
          that no knots would normally be allowed (endspan and minspan overlap).
        o Added the Adjust.endspan argument to reduce the possibility of an
          overfitted interaction term supported by just a few cases on the
          boundary of the predictor space.

    variance models:
        o Variance model residuals now use the predicted value instead of the
          mean out-of-fold values.  This is simpler, and gives better results
          in simulation.  A consequence is that the legal values for
          predict.varmod's type argument have changed.
        o The convergence criteria for residual model IRLS was modified and
          non-convergence is better reported.

    The intercept-only RSS with weights is now calculated correctly in earth.c.
    Weights are now better tested.

    The Get.crit argument was removed from earth.fit.  No longer needed.

    RSq's and GRSq's that are within 1e-5 of zero are now set to zero to facilitate
    testing across machines in the presence of numerical error,  especially
    when case weights are used.

    Increased MaxAllowedRssDelta in earth.c because it was a bit too conservative.

    Updated the vignettes and help pages, and some error messages.

## 4.1.0  Dec 17, 2014

    Added clang to the slowtests suite, and cleaned up minor warnings issued by clang.

    Forward pass termination messages are now similar in R and C code.

    The internal variable "reason" is now named "termcond".

    Documentation improvements.

## 4.0.0  Dec 12, 2014

    Earth now supports prediction intervals.  See earth's new var.method
    argument, and the new vignette.  The predict.earth function now has
    "interval" and "level" arguments.  The residual plot of plot.earth now
    shows prediction intervals if available.  The new version of plotmo
    will also show earth prediction intervals.

    Case weights are now supported but are not yet comprehensively tested.
    The current implementation of case weights is slow.

    The print.earth and print.summary.earth function now print the reason
    the forward pass terminated.

    The ordering of terms in summary.earth was changed slightly: within
    term pairs, the term for "predictor less than hinge" is now placed
    before the term for "predictor greater than hinge" (so for example,
    h(16-Girth) is before h(Girth-16)}.  This was done to make model
    interpretation slightly easier.

    Predictors that are discovered to best enter linearly (no knot) are
    now printed without a knot by summary.earth, to make model
    interpretation a little easier.  Also, their entry in the dirs matrix
    is now 2, the same as predictors that are forced to enter linearly
    by earth's linpred arg.

    The minspan argument can now take negative values, to specify the
    max number of knots per predictor (as opposed to the spacing between
    the knots).

    Earth now has an endspan argument.  The default endspan=0 gives
    the previous behaviour.

    The linpreds argument now accepts variables specified by name.

    Earth now accepts nk=1 to force an intercept-only model (in previous
    versions, nk had to be at least 3).

    DUP is now always TRUE in internal calls to C function, to satisfy
    CRAN checks.  Sadly, this means that the earth forward pass uses
    more memory (because all the argument data is duplicated when
    handing over to C).

    The comprehensive (but slow) earth tests are now in inst/slowtests.
    As always, the tests in earth/tests just do a basic test of
    portability problems.

    "cv.rsq" is now consistently "CVRSq".

    In the earth C code, QR_TOL is now 1e-7 (was .01) to match
    lm's use of tol=1e-7 for dqrdc2.

    The GLM types for residuals.earth are now prefixed by "glm.",
    and type="pearson" now returns the pearsonized residuals.

    Various other minor tweaks, improvements to error messages, etc.

    Changes to plot.earth:

      plot.earth has some new arguments, including info,
      delever, pearson, level, and versus.

      The which argument now has extra values 5:8, to plot
      residuals vs log fitted values etc.

      The residuals plot now has a symmetrical y axis to make asymmetry
      in the point cloud more obvious (see the center argument).

      Graphics arguments to plot.earth are moved down so the more
      important arguments are earlier.

      plot.earth now has an xlim argument, and the ylim argument can
      now be used on any plot (not just the Model Selection plot),
      providing it is the only plot (length(which == 1).

      Some args are deprecated or changed (you will get an
      informative message if you use them).  The changes were
      made for consistency with similar arguments elsewhere.
          --Old--        --New--
          col.rsq        col.line
          col.residuals  col.points
          nresiduals     npoints
          col.legend     use legend.pos=NA for no legend

      If 1 is in the "which" arg to plot.earth, then ylim is used
      for the Model Selection plot, else it is used for the
      Residuals vs Fitted plot.

## 3.2-7  Jan 28, 2014

    Tweaks to standalone version of earth for clean compiles with certain 64 GCC builds.
    plot.evimp now works with intercept-only models.
    Other minor tweaks to plot.evimp.
    Clerical changes to satisfy recent CRAN checks.

## 3.2-6  Apr 15, 2013
    Added format.earth style="C".
    Included the leaps package files into the earth package, to prevent
      warnings from CRAN check about earth's use of internal leaps code.
      Removed "Depends: leaps" from earth's DESCRIPTION file.
    Lin deps discovered by leaps are now slightly better handled.
    Small changes to earth.c and earth.h for stand-alone builds.
    Added sections to the FAQ on using earth for
      binary and categorical responses.
    Added a section to the vignette on building a model
      based on cross validation results.
    Removed trace=4 from test.earth.R so a non-problem is no longer
      reported due to minor numerical differences across architectures.
    Reduced the number of digits printed in some of earth's trace messages.
      This makes trace results more consistent across different architectures
      (the ls digits are often in the numerical noise floor).
    We now print a max of 20 columns of a matrix when tracing.
    The legend.text arg of plot.earth.models now works correctly.

## 3.2-3  Apr 27, 2012
    Incorporated Glen Luckjiff's patch to fix predict.earth(type="terms")
      failure when bx had just two columns.
    We no longer call fflush when in an R environment.
    The tests/test.earth.R code now uses the tree rather than the ozone1 data,
      in an effort to get test consistency across different architectures.
    Added .Rinstignore to drop inst/docs/figs files (prevents note in CRAN build).

## 3.2-2  Mar 14, 2012
    We no longer pass R_NilValue from the R code to the C code.
    Incorporated Olaf Mersmann's patch for POS_INF with the INTEL_COMPILER.
    Code touchups for Microsoft Visual Studio 2010.

## 3.2-1  Jul 19, 2011
      Documentation touchups.

## 3.2-0  Jun 19, 2011
    The PDF file is now a standard vignette (but with no executable code).
    The "Importances" printed by print.earth are now a maximum of option("width") wide.
    We no longer warn "effective number of GCV parameters >= number of cases"
       but instead set such GCVs to Inf, so GCVs are now always
       non-decreasing with the number of terms.  Documented as FAQ 12.11.
    The earth C routines now print memory allocations if trace=1.5.
    The pnTrace arg to ForwardPassR is now a double and called pTrace (allowing trace=1.5).
    The default nk is now clipped at 200.  This limits the amount of memory
       needed in the forward pass for large x matrices.  Numerical limits
       will probably kick in before 200 are reached anyway.
    We now force Use.beta.cache=FALSE with a message if the beta cache would
       be more than 3 GB.
    summary.earth now warns if illegal arguments are passed.
    Documentation touched up.

## 3.1-1  Jun 15, 2011
    Documentation emendations.
    We now issue a warning if user uses a CV arg to plot.earth but no CV data to plot.
    The CV summary print no longer prints the response name for single response models.

## 3.1-0  Jun 14, 2011
    print.evimp now formats the results a little differently (more clearly).
    "Reached min GRSq" and similar trace messages now also print the number of terms.
    The min auto-calculated ylim in model selection plots is now clamped at -1.
    Added the col.residuals, col.pch.max.oof.rsq, and col.pch.cv.rsq args to plot.earth.
    Cross validation with wp no longer gives an error message.
    Revised the documentation.

## 3.0-0  Jun 10, 2011
    Created the vignette earth-notes.pdf which collects and extends
     documentation that was previously in the help files.
    Revamped cross validation.  New argument "ncross" to earth.
    Extended plot.earth, mostly for cross validation.
    The default for evimp's "sqrt." argument is now TRUE.
    plot.earth now uses lowess rather than loess (loess tends to issue ugly warnings).
    Removed earth's Print.pruning.pass argument.
    Earth now uses less memory if passed an x matrix of doubles.  Earth's memory report
     (with trace>=4) was not accurate in all cases and has been removed.
    Tweaked the Author: entry in DESCRIPTION for better results from citation().
    Other minor touchups.

## 2.6-2  Apr 25, 2011
    Moved plotmo.methods.earth.R to here so the plotmo package is independent of earth.
    Modified test scripts to conform to R 2.13.0's way of printing numbers.

## 2.6-1  Apr 13, 2011
    Fixed an issue of dependency on the plotmo package.

## 2.6-0  Apr 12, 2011
    Moved plotmo to the plotmo package, and added "depends plotmo" to earth's DESCRIPTION
    Added the Exhaustive.tol arg as a work-around for leaps.exhaustive error code -999
      (Thanks to David Marra for help tracking this down.)
    The "Exhaustive pruning" pacifier is now printed based on the number of subsets
    Warnings from leaps routines are now treated as errors (because ret val is bad with warning)
    Replaced n=2 and similar in calls to eval with an explicit environment
    The decision to use eval.subsets.xtx is now more automatic and less obtrusive
    The response y is now named in certain situations where it used to be unnamed
    Removed annoying warning "specified nprune 20 is greater than the number of model terms"
    Some code tidying

## 2.5-1  Mar 25 2011
       Modified handling of illegal arguments

## 2.5-0  Mar 24 2011
    Changes to plotmo:
     The arguments were reordered (moved the most important arguments first).
     The ycolumn arg is now called nresponse and handled more consistently.
     The degree2="all" setting is now replaced by all2=TRUE, likewise for degree1.
     The default for the type argument is now object dependent.
     The cex parameter is now supported, and text is a little larger for most plots.
     The degree2 graph titles now have a space for readability.
     There is some minor backwards incompatibility in the minor arguments.
     The plotmo graphs are now ordered on the variable order, for all model classes.
     Plotmo now handles a wider range of object classes.
     Factor predictors are now plotted in degree2 plots.
    Miscellaneous other touchups.

## 2.4-8  Mar 05 2011
    plotmo now handles vector colors better
    plotmo is now better at plotting lda and qda models
    Added the jitter.response argument to plotmo
    Moved plotmo method functions into a new file, plotmo.methods.R.

## 2.4-7  Feb 02 2011
    Intercept-only models are now better supported, esp. for earth-glm  models.
    Martin Renner and Keith Woolner provided the impetus for this change.

    Legend arguments are now handled better in plot.earth.models.
    A vector legend.text is now handled correctly and you can use
    values like "topleft" in legend.pos (thanks to Gavin Simpson at ECRC).

    The elements of cv.list in earth's returned value are now named,
    and plot.earth.models(model$cv.list) thus has better legends.

    Cumulative distrib plots in plot.earth.models now use the jitter arg, and
    plot.earth's (pointless) jitter argument has been removed.

    A few code and doc touchups were also made.

## 2.4-6 Jan 26 2011

    Made the following changes to plotmo:
      We now print the fixed grid values unless trace < 0.
      The degree1 and degree2 arguments now have an "all" option.
      We now have better support for rpart objects
      The cex arg is now also used for response values (with cex.response!=0).
      Added cex.lab argument.
      For factor predictors we now print the factor levels vertically along the x axis.
    Fixed crossed type.gcv and type.rss args in plot.evimp.
    Fixed residuals.earth to handle vector responses correctly.
    (Thanks to Gavin Simpson at UCL for providing fixes for the above bugs.)
    plot.evimp now doesn't use on.exit(par(no.readonly=TRUE)), so we
      now allow subsequent plots on the same page with mfrow != c(1,1).
    Did the usual document touchups.
    Minor cleanups for R check (partial arg matches).

## 2.4-5 Oct 30 2010

    Plotmo now better handles the "data" argument for earth.formula models.
    (Thanks to Keith Woolner for spotting that this was previously incorrect.)

## 2.4-4 Oct 6 2010
    NAs are now allowed in the data passed to predict.earth.  The predicted value
    will be NA unless the NAs are in variables that are unused in the earth model.

## 2.4-3 Sep 29 2010
    For certain data we were seeing duplicated term names for cuts that
    are very close to each other.  To avoid this you can now use
    options(digits) to increase the number of significant digits used
    when forming term names.  (Thanks to Keith Woolner for spotting this.)

## 2.4-2 Aug 28 2010
    We now give the correct error message for attempted cross validation of
        paired binomial responses (which is not yet supported).
    Labels on the largest residuals in plot.earth graphs are now slightly
        better positioned (using plotrix::thigmophobe.labels).
    plotd now handles qda objects in the same manner as lda objects
    Other doc touchups.

## 2.4-0 Nov 8 2009
    This version of earth will build models SLIGHTLY DIFFERENT FROM PRIOR VERSIONS,
    because I fixed a bug in the calculation of minspan and endspan.
    Use minspan=-1 for backwards compatibility.
    (Thanks to Gints Jekabsons for spotting this.)

## 2.3-5 Oct 7 2009
    Fixed bug where glm.control args were ignored under certain circumstances
    (Thanks to Jerome Guelat for letting me know about this.)

## 2.3-4 Sep 21 2009
    . Fixed bug where predict.earth of a earth-glm model under R 2.9.0 and higher
      incorrectly gave the message: Object of type 'closure' is not subsettable
      (Thanks to Thomas Brockmeier for help tracking this down.)
    . plotd now correctly ignores the vline if vline.col=0
    . Doc touchups

## 2.3-2 Mar 23 2009

    Extended plotd
    Doc touchups

## 2.3-1 Feb 26 2009
    Doc touchups
        Added leaps::: prefix needed for new version of the leaps package
        Added style="max" to summary.earth
        Added labels param to plotd
        Reordered some of earth's arguments

## 2.3-0 Feb 18 2009
    Added the plotd function
    Removed some restrictions on type="class" in predict.earth
    Added sqrt. argument to evimp
    Added more grid lines to cumul density plot in plot.earth
    Added a help page section on interpreting the graphs in plot.earth
    Miscellaneous other touchups to code and docs

## 2.2-3 Feb 2 2009
    Added levels to the return value
    Added type=class and thresh arguments to predict.earth
    Thanks to Max Kuhn for suggesting these improvements

## 2.2-2 Jan 30 2009
    Doc touchups
    Cross validation MaxErr is now signed

## 2.2-1 Jan 22 2009
    Added cross validation i.e. the nfold and stratify parameters.

    We now scale y before the forward pass, for better numeric stability in
    the forward pass with very big or very small y's.
    For the old behaviour, set earth's new argument scale.y=FALSE.

    Added get.pairs.bagEarth so plotmo prints degree2 plots for caret:bagEarth models.

    Changes internal to earth.c:
        High values of trace argument are treated differently
        ServiceR (to allow interrupts) is called more consistently for large datasets
        Delta RSS handling is simplified
        Changed some var names for consistency
    The last two were a byproduct of experimental changes to earth that
    were not included in this release.

    Changed documentation to American English.

## 2.1-2 Nov 18 2008
    Touched up evimp help page.

## 2.1-0 Nov 15 2008
    plotmo now has better support for factors and for glm models
    na.action (always na.fail) is now handled as documented in earth.formula
    Added style="bf" to format.earth and summary.earth
    Fixed a few minor bugs and touched up documentation for evimp

## 2.0-6 Oct 30 2008
    You can now pass only the needed subset of columns to predict.earth
    Added plot.evimp
    Removed spurious warning "Need as many rows as columns"

## 2.0-5 Jul 14 2008
        Touched up documentation for format.lm.

## 2.0-4 June 22 2008
        Touched up code and documentation for a zero thresh value.

## 2.0-3 June 22 2008

    Zero values are now allowed for earth's "thresh" parameter (previously if
        you used thresh=0, thresh was clamped internally to 1e-10).
        Also, if thresh=0, the MAX_GRSQ forward pass condition is ignored.
        The idea is to get as close to a big nk as possible
    Changed "valid.names" argument of format.earth and format.lm to "colon.char"
        which achieves the same end more simply.

## 2.0-2 June 15 2008

    Added column names to results of mars.to.earth, allows use of evimp.
    Added valid.names argument to format.earth and format.lm.

## 2.0-1 June 10 2008

    Added "namesx" and "first" arguments to the "allowed" function.
    evimp() for a scalar x now returns a matrix (I added a missing drop=FALSE).

## 2.0-0 June 07 2008

    Added support for glms and factors (but plotmo does not yet support factors).
    Added variable importance function "evimp".
    Added response weights argument "wp" to earth.
    Output of summary.earth has changed to better deal
       with multiple response models, see the "style" argument.
    Added namesx and namesx.org to earth's return value.

## 1.3-2 Mar 29 2008

    Fixed two bad multiple response bugs:
    a) for multiple reponse models, earth calculated the wrong null RSS and
       therefore the wrong RSq and GRSq for the sub-models.  (The total
       RSq and GRSq were correct.)
    b) the wrong betas were used when pruning multiple response models.

    Also fixed a bug where summary.earth printed the wrong number
    of cases for multiple response models.

## 1.3-1 Mar 22 2008

    "update.earth" now has a "ponly" argument, to force pruning only.
    Because of this change, the "ppenalty" argument to earth is no longer
    needed and has been removed.

    Revisited text of warnings after I was bamboozled by one of
    my own warnings.

    Tweaked legend positioning in plot.earth.models.

## 1.3-0 Mar 18 2008

    Default minspan is now 0 (was 1) for compatibility with mda:mars and
    Friedman's MARS paper (I've flip flopped on this one).  This means
    that models built with the default args will be little different to
    before.

    Earth's peak memory is now about 40% less.

    Big models are now more responsive to ^C.

    For multiple response models, we now print response names in most
    places instead of just "Response N".

    Removed get.nterms.per.degree and get.nused.preds.per.subset from
    NAMESPACE and from help pages, to simplify user interface.

    Fixed some niggling document issues and extended the FAQ.

## 1.2-2 Jan 2008

    print.summary.earth now prints the call even for x,y interface to earth
    plotmo now accepts x matrices without column names
    Tweaked FindKnot and OrthogResiduals for speed
    Removed a few shadowed variables in earth.c after running gcc -Wshadow
    Clarified some paras in earth.Rd, reduced page width for better html display

## 1.2-1

    Fixed a newvar.penalty bug introduced in previous release.
    Added src/tests/test.earthmain.gcc.bat
    More man page tweaks

## 1.2-0

    Added linpreds, allowed, and Use.beta.cache arguments
    Anova decomp is now more consistent
    Added a few GPL headers
    Reinstated the beta cache
    More man page tweaks

## 1.1-5

    Fixed bug reported by Joe Ritzer: long predictor names got munged in plotmo
    Changed "class" to "response" throughout when used for the
    predicted responses in the input y.
    Man page tweaks based on user feedback.

## 1.1-4

    Changed as.matrix to data.matrix in earth.default -- grep for FIXED
    Extended earth.Rd slightly

## 1.1-2

    Added my web page to DESCRIPTION and to some man pages

## 1.1-1

    Changed \r\n to \n to pacify CMD CHECK

## 1.1-0

    Default minspan is now 1 (was 0)
    Fixed potential crash in PrintForwardStep if nTrace>1
    Added a missing drop=FALSE to backward()
    Minor code, comment, and man page fixups

## 1.0-8

    Fixed bug where plotmo failed under these circumstances:
        form <- Volume ~ .; a <- earth(form, data = trees); plotmo(a)

## 1.0-7

    Minor change to summary.earth formatting.  Man page fixes.

## 1.0-6  May 11 2007

    Initial release
