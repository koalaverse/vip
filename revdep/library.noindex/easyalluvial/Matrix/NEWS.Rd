% Check from R:
%  news(db = tools:::.build_news_db_from_package_NEWS_Rd("~/R/Pkgs/Matrix/inst/NEWS.Rd"))
\name{NEWS}
\title{News for \R{} Package \pkg{Matrix}}% MM: look into ../svn-log-from.all
\encoding{UTF-8}
%% NB: The date (yyyy-mm-dd) is the "Packaged:" date in ../DESCRIPTION

\section{Changes in version 1.5-4.1 (2023-05-16)}{
  \subsection{Misc}{
    \itemize{
      \item Use \code{#ifdef PR18534fixed} to adapt to \R{}'s Lapack
      header fix for \PR{18534}.
    }
  }
}

\section{Changes in version 1.5-4 (2023-04-02 r3837)}{
  \subsection{Deprecated and Defunct}{
    \itemize{
      \item The following low level coercion utilities, which were
      previously exported but always \dQuote{hidden} and undocumented,
      are now deprecated:
      \code{..2dge}, \code{.C2nC}, \code{.T2Cmat}, \code{.asmatrix},
      \code{.dense2sy}, \code{.diag2mat}, \code{.diag2sT},
      \code{.diag2tT}, \code{.dsy2dsp}, \code{.dsy2mat}, \code{.dxC2mat},
      \code{.m2dgC}, \code{.m2lgC}, \code{.m2ngC}, \code{.m2ngCn},
      \code{.m2ngTn}, \code{.n2dgT}, \code{.nC2d}, \code{.nC2l}.
      \cr
      The deprecations follow efforts to define more general and more
      systematic (but still fast) coercion utilities, to allow expert
      users to bypass S4 dispatch in more cases.  The subset of these
      currently exported is documented under \code{help("fastMisc")}.
      Deprecation warnings will suggest an appropriate replacement,
      mostly from that list.
      \cr
      It is not always the case that the replacement can be
      \dQuote{dropped in}, hence users should consult the relevant
      documentation when changing their code.

      \item Warnings signaled by coercion methods deprecated
      in \pkg{Matrix} 1.5-0 now correctly inherit from class
      \code{deprecatedWarning}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Defining \code{sequence.default} for \R{} \code{< 4.0.0},
      so that \code{kronecker(<CsparseMatrix>, <CsparseMatrix>)},
      \code{kronecker(<CsparseMatrix>, <diagonalMatrix>)}, and
      \code{<diagonalMatrix> \%*\% <denseMatrix>} continue to work
      there.
    }
  }
  \subsection{Misc}{
    \itemize{
      \item C-level utilities \code{Matrix_memset()} and
      \code{Matrix_memcpy()}, both new, are now used in many places
      instead of API macros \code{Memzero()} and \code{Memcpy()}
      which never check for overflow.  C-level macro \code{AZERO()}
      is no longer defined.

      \item C-level macro \code{Calloc_or_Alloca_TO()} now
      zero-initializes also in the \code{alloca}-using case.

      \item Replace deprecated \code{Rdefines.h} with
      \code{Rinternals.h} and move inclusion outside of
      \code{extern "C"} wrapper in API header
      \file{inst/include/Matrix.h}.

      \item Replace \code{sprintf} with \code{snprintf} globally
      to avoid potential buffer overflows.
    }
  }
}

\section{Changes in version 1.5-3 (2022-11-10 r3772)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{dimScale(x)} with argument \code{d1} missing is no
      longer an error.  (The default value had \code{diag(x, FALSE)}
      instead of \code{diag(x, names = FALSE)}.)

      \item \code{(dim|row|col)Scale(x)} is no longer an error for
      traditional matrices \code{x} without a \code{dimnames} attribute.

      \item Our \code{cov2cor()} methods now again preserve (symmetrized!)
      \code{dimnames}, fixing Matrix bug #6783 reported by Ben Bolker.

      \item \code{colSums()} and friends now always give a \emph{named}
      result when the marginal \code{Dimnames} are non-\code{NULL}.
      (Names were \dQuote{forgotten} for \code{diagonalMatrix} and
      \code{indMatrix} arguments.)

      \item \code{colSums()} and friends now respect \code{na.rm} when
      handling \code{diagonalMatrix} with \code{NA} diagonal entries.

      \item \code{expand(<denseLU>)} now \dQuote{copies-on-modify},
      no longer duplicating the \code{m*n}-length \code{x} slot in
      the \code{m == n} case, when it can be used directly.

      \item \code{lu(<m-by-0>)}, \code{lu(<0-by-n>)}, and
      \code{BunchKaufman(<0-by-0>)} now give sensible (0-extent)
      results, rather than a LAPACK error, for \code{denseMatrix}.
    }
  }
  \subsection{New Features}{
    \itemize{
      \item \code{Diagonal()} gets a new optional \code{names} argument.

      \item \code{diag(x) <- value} is now done in C also for
      \code{[CRT]sparseMatrix}.

      \item \code{.diagU2N()} gets fast counterpart \code{.diagN2U()}.

      \item \code{colSums()} and friends are now implemented more
      efficiently for \code{denseMatrix} and \code{[CRT]sparseMatrix}.
      Notably, methods for triangular and symmetric matrices no longer
      go via \code{generalMatrix}, and methods for
      \code{[CRT]sparseMatrix} now handle nonzero pattern and logical
      matrices directly (no longer coercing to double, a constraint
      of the earlier implementation using CHOLMOD).

      \item \code{determinant(<ds[yp]Matrix>)} is now computed via
      the Bunch-Kaufman factorization.  Factorizations are cached in
      the \code{factors} slot for future reuse.
    }
  }
  \subsection{Misc}{
    \itemize{
      \item Package \pkg{methods} has been moved from \code{Imports}
      to \code{Depends}, as suggested in the WRE manual.  Now
      \code{as()} and other basic S4 machinery are available whenever
      \pkg{Matrix} is attached.  This change affects R processes
      started with environment variable \code{R_DEFAULT_PACKAGES} set
      to \code{NULL} (or some list not containing \pkg{methods}).

      \item \file{Simple.R} test for sparse \code{"POSIXlt"} example
      adapted to latest R-devel always having \code{"zone"} which is
      character.

      \item C-level wrappers for LAPACK \code{d..trf} routines gain
      an argument \code{warn} indicating how to handle \code{info > 0}:
      \code{warn <= 0} is silent, \code{warn = 1} is a warning, and
      \code{warn > 1} is an error.  In the case of \code{dp[op]trf},
      for which \code{info > 0} implies an incomplete factorization,
      \code{info} is now returned as a length-1 integer.
    }
  }
}

\section{Changes in version 1.5-2 (2022-10-21 r3702)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item C-level functions now \code{PROTECT()} the result of
      \code{R_do_slot()}, \code{getAttrib()}, \code{mkString()}, etc.
      in many more (but not yet all) places, resolving many new
      and some old \code{rchk} warnings.

      \item \code{lu(x)@L@uplo} is now \code{"L"}, not \code{"U"},
      for 0-by-0 and 1-by-1 \code{dgCMatrix} \code{x}.

      \item The validity methods for classes \code{l[ts]CMatrix}
      now correctly test for structurally nonzero entries on the
      wrong side of the diagonal, and fail in that case.  This test
      was previously only performed for \code{d[ts]Matrix}.

      \item The validity and initialization methods for virtual class
      \code{sparseVector} are more diligent, i.e., catching more edge
      cases such as \code{NA} in the \code{length} or \code{i} slot.

      \item The validity method for \code{corMatrix} now throws a better
      error when the \code{sd} slot is of type \code{"integer"} rather
      than \code{"double"}.

      \item \code{.sparseDiagonal()} now agrees with \code{Diagonal()}
      when called with no arguments, returning a 0-by-0 (rather than
      1-by-1) diagonal \code{Matrix}.

      \item \code{sparseMatrix(i, j)} with 0-length \code{i} and
      \code{j} now returns a 0-by-0 matrix rather than throwing a
      perplexing error.

      \item \code{sparseMatrix(dims = <list>)} and
      \code{sparseMatrix(x = <dMatrix>)} now produce errors.

      \item \code{diag(x) <- value} now coerces \code{diagonalMatrix}
      \code{x} if \code{typeof(value)} is \dQuote{higher} than
      \code{typeof(x@x)} in the usual hierarchy, for consistency with
      methods for \code{denseMatrix} and with \code{base::`diag<-`}.

      \item Methods for \code{kronecker()} no longer ignore the
      \code{make.dimnames} argument.

      \item Printing a \code{sparseMatrix} with \code{NA} row or column
      names is no longer an error.

      \item Products of two \code{pMatrix} objects \code{x} and \code{y}
      are now computed correctly.  Previously, \code{y \%*\% x} was
      returned instead of \code{x \%*\% y}!

      \item Products \code{x \%*\% y} with \code{x} a
      \code{diagonalMatrix} or \code{indMatrix} and \code{y} a
      traditional matrix or vector, or with \code{x} a traditional
      matrix or vector and \code{y} a \code{diagonalMatrix}
      or \code{pMatrix}, now treat the unclassed argument as a
      \code{.geMatrix} and return a \code{.geMatrix}, for greater
      consistency with other products involving one \code{Matrix}
      and one non-\code{Matrix}.

      \item Similarly, \code{kronecker(x, y)} with one of \code{x} and
      \code{y} a \code{Matrix} and the other a traditional matrix or
      vector now treats the unclassed argument as a \code{.geMatrix}.

      \item \code{dimnames(solve(x))} is now \code{rev(dimnames(x))}
      for \code{denseMatrix} \code{x}, consistent with the handling
      of \code{dimnames} by \code{solve.default}.  Methods for
      \code{sparseMatrix} \code{x} have not been addressed (yet).
    }
  }
  \subsection{New Features}{
    \itemize{
      \item \code{is.nan(x)} is now implemented for all \code{x}
      inheriting from virtual class \code{Matrix} or \code{sparseVector}.

      \item New C-level validity methods for \code{n[ts][CRT]Matrix},
      \code{p?Cholesky}, \code{p?BunchKaufman}, \code{Schur},
      \code{denseLU}, \code{sparseLU}, and \code{sparseQR}.
      Notably, in the \code{MatrixFactorization} cases, the following
      properties are now checked: the dimensions of each factor,
      the orientation of each triangular (or trapezoidal) factor, and
      the validity of each permutation vector.

      \item \code{Diagonal(n=, x=)} now recycles \code{x} of any
      positive length to length \code{n}.  Previously, recycling was
      supported only for \code{x} of length 1.

      \item Products involving \code{diagonalMatrix} or \code{indMatrix}
      have been broadly improved as follows:
      \itemize{
	\item \code{dimnames(A \%*\% B)} is now always
	\code{c(dimnames(A)[1], dimnames(B)[2])}.

	\item \code{t?crossprod()} methods involving \code{indMatrix}
	or its subclass \code{pMatrix} gain a \code{boolArith} argument.

	\item Numeric and boolean products are always returned as
	\code{dMatrix} and \code{nMatrix}, respectively, except in
	a few special cases where the product can be represented
	as an \code{indMatrix}.  (Previously, coercions were skipped
	when one of the operands was unit diagonal.)

	\item Products of \code{diagonalMatrix} with dense
	\code{triangularMatrix} now correctly give a
	\code{triangularMatrix} result (and without unpacking).

	\item Products of \code{diagonalMatrix} with
	\code{[RT]sparseMatrix} now preserve storage, no longer
	coercing to \code{CsparseMatrix}.

	\item \code{crossprod(x, y)} no longer requires the product of
	\code{ncol(x)} and \code{ncol(y)} to be less than \code{2^31}
	when both \code{x} and \code{y} are \code{indMatrix}.  (The
	new implementation now always gives a \code{dgTMatrix} result,
	whereas previously the result would typically but not always
	be a \code{dgCMatrix}.)
      }

      \item \code{kronecker(<Matrix>, <Matrix>)} now gives the correct
      \dQuote{shape} (general, [unit] triangular, symmetric, diagonal)
      in all cases where it can be known without checking.

      \item \code{kronecker(<[CR]sparseMatrix>, <Matrix>)} now retains
      the storage of the first argument, no longer coercing to
      \code{TsparseMatrix}.

      \item New exported functions \code{dimScale}, \code{rowScale}, and
      \code{colScale}, for scaling rows and columns of a \code{[mM]atrix}
      without losing \code{dimnames} and where appropriate without
      losing symmetry.
    }
  }
}

\section{Changes in version 1.5-1 (2022-09-12 r3642)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item ASAN-detected bugs fixed in C-level functions
      \code{Tsparse_as_CRsparse()}
      (triggered by \code{.T2C(<0-by-0>)})
      and \code{pMatrix_validate()}
      (triggered by \code{as(<length-10000 integer>, "pMatrix")}).
    }
  }
}

\section{Changes in version 1.5-0 (2022-09-09 r3636)}{
  \subsection{Deprecated and Defunct}{
    \itemize{
      \item With a few exceptions, direct coercions to non-virtual subclasses
      of \code{Matrix} (e.g., \code{dsCMatrix}) have been formally deprecated.
      For now, these will continue to work as before, but with a warning
      indicating how to accomplish the desired coercion via virtual classes
      (e.g., \code{symmetricMatrix}) alone.  How such warnings are signaled
      is controlled by the global option \code{Matrix.warnDeprecatedCoerce}.
      \describe{
	\item{\code{0} or less}{is to be silent.}
	\item{\code{1}}{is to signal a warning with each deprecated coercion.}
	\item{\code{2} or greater}{is to signal an error with each
	  deprecated coercion.}
	\item{\code{NA}}{is to signal a message or warning (see below)
	  with the next deprecated coercion and be silent after that.}
      }
      If unset or invalid, then the value of the environment variable
      \env{R_MATRIX_WARN_DEPRECATED_COERCE} (\code{NA} if unset) is
      used.  This is cached when the \pkg{Matrix} namespace is loaded.
      \cr
      Option values are coerced to integer before use.
      \cr
      To reduce disruption to existing code, the \code{NA} case signals
      messages rather than warnings with coercions to the most-used
      non-virtual subclasses of \code{Matrix}, namely \code{dg.Matrix}
      and \code{d.CMatrix}.  This may change in the future.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Symmetrization of \code{<Matrix>@Dimnames} and
      \code{dimnames(<matrix>)} now goes through C utility \code{symmDN()}
      in most places, resolving some earlier inconsistencies.

      \item Many more validity methods now correctly operate under the
      assumption that methods for superclasses have already been called,
      eliminating many redundant checks.

      \item Validation of \code{<Matrix>@Dim} now looks at type
      \emph{before} length, avoiding a misleading error message.

      \item Validation of \code{<Matrix>@Dimnames} now avoids
      \code{isNewList}, which had allowed an error message suggesting
      that \code{NULL} is a list.

      \item Setting a factor on a \code{compMatrix} is now to install
      the factor itself, not a copy, for efficiency and consistency
      with the semantics of \code{<compMatrix>@factors[[name]] <- value}.

      \item Long vector support in methods for packing and unpacking
      \code{denseMatrix}, and others.

      \item \code{diag<-} incorrectly preserved the class of dense matrices,
      so that, e.g., \code{`diag<-`(x=<dpoMatrix>, value=-1)} was still a
      \code{dpoMatrix}.  Now the result is always one of the more general
      \code{.(ge|tr|sy|tp|sp)Matrix}.

      \item \code{t(<corMatrix>)} no longer clears the \code{sd} slot.
      \code{t(<p?BunchKaufman>)} now returns one of the more general
      \code{dt[rp]Matrix}, rather than preserving class and clearing
      the \code{perm} slot.

      \item \code{t(<symmetricMatrix>)} no longer reverses the
      \code{Dimnames} slot.  Symmetrization of \code{dn <- x@Dimnames}
      and \code{t(x)@Dimnames} had given different results when
      \code{dn[[1]]} and \code{dn[[2]]} were non-\code{NULL} and
      asymmetric.

      \item \code{isTriangular(x, upper)} had incorrectly returned
      \code{FALSE} for \code{x} of class \code{triangularMatrix}
      when \code{upper = TRUE} and \code{x@uplo = "L"} or when
      \code{upper = FALSE} and \code{x@uplo = "U"}.
      \code{isTriangular} is now equivalent to \code{isDiagonal}
      in those cases.

      \item \code{isSymmetric()} was equivalent to \code{isDiagonal()}
      for triangular matrices, \emph{not} allowing numerical fuzz via an
      argument \code{tol}.  A \code{tol} argument is now implemented
      for all subclasses of \code{dMatrix} except for those inheriting
      from \code{symmetricMatrix} or \code{diagonalMatrix}.

      \item Methods for \code{isSymmetric} now also look at
      \code{Dimnames} and \code{names(Dimnames)}, following
      \code{isSymmetric.matrix} from \pkg{base}.  See also
      New Features.

      \item \code{band(x, -k, k)} for sparse \code{x} used
      \code{isSymmetric(x)} (which tolerates numerical fuzz) to test
      for symmetry, resulting in loss of information in some cases.
      Now it tests that \code{x} inherits from \code{symmetricMatrix},
      and so returns \code{symmetricMatrix} in fewer cases.

      \item \code{triu(x, k)} and \code{tril(x, k)} incorrectly
      required \code{k <= m} (instead of \code{k <= n}),
      for \code{m}-by-\code{n} sparse \code{x}.  They now accept
      all \code{k} from \code{-m} to \code{n}, with fewer errors
      in the \code{m < n} case.

      \item \code{crossprod(<Rsparse>, <Tsparse>)} and similar now work
      again (optional \code{boolArith} was not passed on), fixing Matrix
      bug #6766 by David Cortes.
      Ditto for \code{tcrossprod()}, where the old result was even wrong
      when it had \dQuote{worked}, before Matrix 1.2-0.

      \item \code{as(<matrix>, "nMatrix")} can now be sparse \emph{or}
      dense, going via \code{Matrix()}, for greater consistency with
      coercions to \code{dMatrix} and \code{lMatrix}.
      (Previously, the result was always an \code{ngTMatrix}.)

      \item \code{forceSymmetric(<[RT]sparseMatrix>)} are now more
      efficient, returning symmetric \code{[RT]sparseMatrix} without
      intermediate coercions to \code{CsparseMatrix}.

      \item \code{tcrossprod(a, b)} for unit triangular sparse matrices
      now works correctly.

      \item \code{!<ltrMatrix>} is no longer an error in the 0-by-0
      unit diagonal case.

      \item Coercions among \code{[CRT]sparseMatrix} preserve the
      \code{factors} slot in more cases.

      \item Coercions of overallocated \code{l.TMatrix} to
      \code{denseMatrix} or \code{CsparseMatrix} now give \code{TRUE}
      instead of \code{NA} in the \code{NA || TRUE} case, following
      conventional logic.

      \item Methods for unpacking and indexing \code{packedMatrix} and
      for coercing from \code{[CRT]sparseMatrix} to \code{denseMatrix}
      now check more consistently for overflow of \code{R_XLEN_T_MAX}.

      \item \code{solve(<ddenseMatrix>, <ANY>)} is removed from
      the method list.  It had allowed infinite recursion, e.g.,
      with \code{solve(new("dgeMatrix"), NULL)}.

      \item \code{is.na(<ndenseMatrix>)} gave \code{TRUE} where
      the \code{x} slot had \code{NA}.  Now the result is always
      a zero matrix.

      \item \code{is.na(<.t[rp]Matrix>)} and
      \code{is.infinite(<.t[rp]Matrix>)} ignored the \code{diag} slot,
      behaving always as though \code{diag == "N"}.  They now give
      \code{FALSE} on the diagonal in the \code{diag != "N"} case.

      \item Now only \dQuote{nontrivial} matrix elements determine
      whether \code{is.na(<denseMatrix>)} is an \code{ndenseMatrix}
      or an \code{nsparseMatrix}.

      \item \code{is.na(<ddenseMatrix>)} coerced to \code{lMatrix}.
      This unnecessary step is avoided now, saving a potentially
      nontrivial allocation.

      \item \code{solve(<ddenseMatrix>, b)} coerced the first argument
      to \code{dgeMatrix} when \code{b} was not a \code{ddenseMatrix}
      or traditional \code{matrix}.  This unnecessary step is avoided now,
      so that specialized methods for \code{d(tr|sy|po|tp|sp|pp)Matrix}
      are used where possible (including for \code{b} inheriting from
      \code{[ln]denseMatrix}, \code{sparseMatrix}, or \code{numLike}).

      \item \code{`dim<-`(x, -x@Dim)} is now an error, no longer
      yielding an invalid \code{Matrix} object.

      \item \code{`dim<-`(x, x@Dim)} is now faster, returning \code{x}
      without allocation in all cases.

      \item \code{`dim<-`(x, value)} gives a better error when
      \code{value} contains \code{NA} or elements exceeding
      \code{INT_MAX}.

      \item \code{`dim<-`(<RsparseMatrix>, value)} is now an
      \code{RsparseMatrix}, rather than a \code{TsparseMatrix}.

      \item For consistency with other methods,
      \code{symmpart(<diagonalMatrix>)} now always inherits from
      both \code{dMatrix} and \code{symmetricMatrix}, and
      \code{skewpart(<diagonalMatrix>)} now always has symmetric
      \code{Dimnames}.

      \item Zeros on the diagonal of \code{skewpart(<[CRT]sparseMatrix>)}
      are now \emph{structural}.

      \item \code{as(<ndenseMatrix>, "(vector|matrix|[dl]Matrix)")}
      and \code{nnzero(<ndenseMatrix>)} now correctly treat \code{NA}
      in the \code{x} slot as \code{TRUE}.

      \item \code{as(<[nl].TMatrix>, "dMatrix")} now correctly handles
      the overallocated case: data for each unique \code{[i,j]}
      pair are aggregated logically (\code{x1 || ... || xn}) rather than
      arithmetically (\code{x1 + ... + xn}), so that elements of the
      result are restricted to the set \code{c(0, 1, NA)}.
      This bug had also affected the result of \code{sum(<[nl].TMatrix>)}.

      \item \code{dimnames(as(x, "matrix"))} is now \code{NULL}
      for \emph{all} \code{x} inheriting from \code{Matrix},
      when \code{x@Dimnames} is the trivial \code{list(NULL, NULL)}.

      \item \code{.bdiag(<named list>)} no longer propagates names
      to the \code{Dim} slot of the result.

      \item \code{as(<named vector>, "denseMatrix")} now correctly
      propagates \code{names} to the result.

      \item \code{as(<d.[CR]Matrix>, "lMatrix")} no longer drops
      non-structural zeros, for greater consistency with analogous
      coercions.

      \item \code{Matrix(x, doDiag)} now behaves as documented
      for diagonal matrices \code{x} with asymmetric \code{dimnames},
      returning a \code{diagonalMatrix} when \code{doDiag = TRUE},
      rather than a \code{triangularMatrix}.

      \item Matrix(<n-dimensional table>) now works for \code{n != 2}.

      \item \code{Matrix(<sparseVector>)} now works for vector lengths
      other than 1, no longer giving an error about length mismatch
      when neither of \code{nrow} and \code{ncol} are supplied.

      \item \code{Matrix(<diagonalMatrix>, doDiag = FALSE)}
      is now a \code{symmetricMatrix}, not a \code{diagonalMatrix},
      matching the documentation of \code{doDiag}.

      \item \code{Matrix(<.geMatrix>, sparse = TRUE, forceCheck)}
      and \code{Matrix(<.g[CRT]Matrix>, sparse = FALSE, forceCheck)}
      now respect \code{forceCheck = FALSE} by always returning
      \code{generalMatrix}, i.e., \emph{not} testing for symmetric
      or triangular structure.

      \item \code{Matrix(0, nrow, )}, \code{Matrix(0, , ncol)} now
      throw (correct) errors for \code{nrow}, \code{ncol} in the
      interval \eqn{[0,1)}, consistent with \code{base::matrix()}.

      \item \code{sparseDefault(<sparseMatrix>)} now counts zeros
      without coercing to \code{matrix},
      making \code{Matrix(<sparseMatrix>, sparse = NULL)} much more
      efficient.

      \item Methods for group generic \code{Math} no longer
      preserve \code{x@diag == "U"} or lose \code{x@Dimnames}
      when \code{f(0) == 0} and \code{f(1) != 1}.
      (The former happened for \code{triangularMatrix} \code{x}
      and the latter for \code{diagonalMatrix} \code{x}.)

      \item \code{image(Z)} for a completely \dQuote{empty} (all 0)
      \code{sparseMatrix} works again (?!).

      \item \code{x[i, ] <- value} and \code{y[, j] <- value}
      is now an error in more cases
      for \code{m}-by-0 \code{CsparseMatrix} \code{x}
      and 0-by-\code{n} \code{CsparseMatrix} \code{y}.
      In these cases, subassignment gave a (formally) invalid result.

      \item \code{chol(<ds[yp]Matrix>)} now calls the underlying
      C-level routine exactly once.  Previously, it was called
      an extra time in order to test for positive definiteness,
      with the result thrown away (!).  Hence these methods should
      become approximately two times faster.

      \item \code{dimnames(chol(x))} is identical to \code{dimnames(x)}
      in all cases, now even when \code{chol(x)} is constructed from a
      cached \code{MatrixFactorization}, for greater consistency with
      \code{base::chol.default()}.

      \item \code{chol(<generalMatrix>)} no longer looks at
      \code{Dimnames} when testing for symmetry.

      \item \code{lu(<dtCMatrix>)} no longer returns an invalid
      \code{sparseLU} object in the lower triangular case.

      \item \code{lu(x)} had sometimes incorrectly cached its
      return value as element \code{"lu"} (rather than \code{"LU"})
      of \code{x@factors}.  Now it is always \code{"LU"}.

      \item \code{"Compare"} operators, e.g., \code{a > b}, \code{x != y},
      now work correctly in more dense unitriangular and sparse 0-extent
      cases.

      \item \code{!<nMatrix>} is now always an \code{nMatrix}, never
      an \code{lMatrix}.

      \item \code{!<ndenseMatrix>} and \code{which(<ndenseMatrix>)}
      now correctly handle \code{NA} as \code{TRUE}.

      \item{anyNA(<denseMatrix>)} had incorrectly returned
      \code{anyNA(.@x)} in many cases, giving false positives
      for some \code{.(tr|sy)Matrix} and \code{ndenseMatrix}.
      Now methods respect the \dQuote{rules} of these classes.

      \item The boolean arithmetic product \code{A \%&\% B} and e.g.,
      \code{crossprod(A, B, boolArith=TRUE)} now should behave as if
      \code{drop0(A)} and \code{drop0(B)} were used, i.e., for formally
      sparse matrices, the boolean product results should be stable with
      respect to non-structural vs structural zeros.

      \item \code{t(<symmetricMatrix>)} now retains the \code{factors}
      slot, avoiding recomputation.

      \item \code{qr(<large sparse>)} no longer segfaults in some cases,
      but now warns about \dQuote{Out of memory} and stops, fixing Matrix
      bug #6610 reported by Benjamin Tyner.

      \item Fixed \code{<sparse> \%*\% <dense>} possible memory corruption,
      visible via valgrind, fixing Matrix bug #6726 reported by David Cortes.

      \item Fixed the (quite \emph{long standing}) Matrix bug #6777,
      reported by Manuel Koller: \code{tcrossprod(<matrix>, <[dln]sCMatrix>)}
      has been wrong in some cases.
    }
  }
  \subsection{New Features}{
    \itemize{
      \item \code{KhatriRao()} gets new \code{sparseY = TRUE} option and
      also works for more \code{Matrix} classes.

      \item Virtual class \code{packedMatrix} gains methods for
      \code{pack}, \code{unpack}, \code{isSymmetric},
      \code{isTriangular}, and \code{isDiagonal} implemented in C,
      replacing those defined for many subclasses individually.

      \item New virtual class \code{unpackedMatrix} containing
      \code{denseMatrix} _without_ \code{packedMatrix}, with methods
      for \code{pack}, \code{unpack}, \code{isSymmetric},
      \code{isTriangular}, \code{isDiagonal}, \code{t}, \code{diag},
      and \code{diag<-} implemented in C, replacing those defined for
      many subclasses individually.

      \item \code{isTriangular} and \code{isDiagonal} are now
      implemented in C also for \code{[CRT]sparseMatrix} and standard
      \code{matrix}.  \code{isSymmetric} is now implemented in C
      for all \code{denseMatrix} and all \code{[CRT]sparseMatrix},
      though these C routines are currently only called when testing
      for \emph{exact} symmetry (always for \code{[ln]Matrix},
      only when \code{tol = 0} for \code{dMatrix}).

      \item Methods for \code{isSymmetric} gain an argument
      \code{checkDN = TRUE} indicating whether symmetry of
      \code{Dimnames} should be checked.  For backwards compatibility
      and consistency with \code{isSymmetric.matrix} from \pkg{base},
      the actual condition is \code{checkDN && check.attributes}.

      \item \code{isTriangular(x, upper)} now has a \code{kind}
      attribute \emph{if and only if} \code{x} is triangular and
      \code{upper} is \code{NA}.

      \item \code{diag(<unpackedMatrix>) <- value} now behaves like
      \code{diag(<packedMatrix>) <- value}, supporting coercions
      depending on \code{typeof(value)}, consistent with \code{diag<-}
      from \pkg{base}.

      \item \code{pack} and \code{unpack} are now identity functions
      for \code{packedMatrix} and \code{unpackedMatrix} arguments,
      respectively (previously an error).  \code{pack(<.geMatrix>)}
      (previously an error) now behaves as \code{pack(<matrix>)},
      i.e., by checking for symmetry or triangularity before packing.
      \code{unpack(<matrix>)} now works and is equivalent to
      \code{as(<matrix>, "unpackedMatrix")}, with result inheriting
      from \code{.(ge|tr|sy)Matrix}, as appropriate.

      \item Many more validity methods implemented in C, for efficiency,
      including methods for \code{Matrix}, \code{compMatrix},
      \code{diagonalMatrix}, \code{indMatrix}, \code{pMatrix},
      \code{corMatrix}, \code{[liz]Matrix}, and \code{ndenseMatrix}.

      \item The validity method of \code{dppMatrix} now follows
      the validity method of \code{dpoMatrix}, requiring non-negative
      diagonal elements.

      \item Validation of \code{<Matrix>@Dimnames[[i]]} now tolerates
      vector types other than character, which are coerced to character
      in C via the new \code{R_DimNames_fixup()}, for greater
      consistency with base \code{matrix()}.

      \item \code{band(x, k1, k2)} is optimized further for both
      dense and sparse \code{x}, returning \code{triangularMatrix},
      \code{symmetricMatrix}, and \code{packedMatrix} in more cases.

      \item \code{band()} is now implemented also for
      \code{diagonalMatrix} (only \code{tri[ul]()} worked before).

      \item Coercions \code{.ge<->.g[CRT]}, \code{.t[rp]<->.t[CRT]},
      \code{.s[yp]<->.s[CRT]}, and \code{..[CRT]<->matrix} are now
      fully implemented and optimized, with minimal intermediate
      allocations.  These (and others) no longer rely on CHOLMOD,
      which had handled integer types as double and required
      preprocessing in many cases (with \code{diagU2N()}, etc.).

      \item Fixes in group methods (e.g., \code{>}, \code{&}, \code{|}),
      notably for matrices inheriting from class \code{symmetricMatrix},
      \code{triangularMatrix}, \code{lMatrix}, or \code{nMatrix}.

      \item \code{as.vector}, \code{as.numeric}, and \code{as.logical}
      are now implemented for all \code{Matrix}.

      \item Subassignment to \code{indMatrix} is no longer prohibited,
      now going via \code{TsparseMatrix}.

      \item \code{indMatrix} gains methods for \code{isTriangular},
      \code{isDiagonal}, \code{diag}, \code{diag<-}, \code{band},
      \code{tri[ul]}, and \code{forceSymmetric}.  It also gains
      coercions to more virtual classes (notably \code{denseMatrix})
      and a coercion to \code{pMatrix}.

      \item \code{solve(<ddenseMatrix>, <numLike>)} now works in all
      cases.

      \item \code{determinant(<ds[yp]Matrix>)} is now much faster
      in the positive definite case, no longer going via \code{dgeMatrix}.

      \item \code{diag(<[CRT]sparseMatrix>)} is now done in C
      and is highly optimized in the \code{.[ts][CR]Matrix} case.

      \item \code{symmpart} and \code{skewpart} are now done in C
      for all \code{denseMatrix} and all \code{[CRT]sparseMatrix}.
      Both now more faithfully preserve the \dQuote{storage} of
      their argument.  (Previously, \code{symmpart(<packedMatrix>)}
      was an \code{unpackedMatrix},
      and \code{(symm|skew)part(<[RT]sparseMatrix>)}
      was a \code{CsparseMatrix}.)

      \item \code{as(<vector|matrix>, "([dln]?dense|[dlnCRT]?sparse)Matrix")}
      are now fully and more consistently implemented.  In the vector case,
      the result is now always a \code{length}-by-1 \code{generalMatrix}.
      In the matrix case, structure is now always examined, hence the result
      is a \code{symmetricMatrix} or \code{triangularMatrix} in more cases.

      \item \code{Matrix(<classed matrix>)} now works for classes
      other than \code{table}.

      \item \code{lu(<dt[rpCRT]Matrix>)} and \code{lu(<ddiMatrix>)}
      now behave more consistently.  In the diagonal, upper triangular,
      and unit lower triangular cases, the result is obtained
      \dQuote{directly}, i.e., without pivoting.  In the non-unit
      lower triangular case, it is obtained with pivoting.
      (Previously, pivoting was never done for \code{dtCMatrix}
      and always done for \code{dt[rpRT]Matrix} and \code{ddiMatrix}.)

      \item \code{lu(x)} now caches its return value also for all
      \code{ds.Matrix} \code{x} (by default).

      \item \code{readMM()} now warns if the number of entries found
      is less than number reported in the header.

      \item \code{x[i]} now works for \code{nMatrix} \code{i},
      just as for \code{lMatrix} \code{i}.  This supports constructions
      such as \code{x[is.na(x)]}, where the logical operation
      produces an \code{nMatrix} because it is never \code{NA}.
    }
  }
  \subsection{Misc}{
    \itemize{
      \item \code{AZERO()} and friends gain an argument specifying
      a zero constant (0 for \code{int} arrays, 0.0 for \code{double}
      arrays).

      \item C-level utilities \code{(R_)?[gs]et_factors()} have been
      renamed \code{(R_)?[gs]et_factor()}, as they only ever get and
      set \emph{one} factor.

      \item The signature of \code{set_factor()} has been changed to
      match other \code{set*()} functions:
      to \code{(object,name,value)} from \code{(object,value,name)}.

      \item For clarity, \code{set_factor()} now returns \code{void}
      and is used like other \code{set*()} functions (i.e., for its
      side effect).  The R interface is unchanged: \code{R_set_factor()}
      continues to return the value being set.

      \item C-level utilities
      \code{make_[di]_matrix_(triangular|symmetric)()},
      \code{packed_to_full_(double|int)()},
      \code{full_to_packed_(double|int)()}, and
      \code{install_diagonal(_int)?()}
      are replaced by safer, more consistently named ones.
      Previous versions allowed integer overflow.

      \item C-level utilities \code{dup_mMatrix_as_d?geMatrix()} are
      replaced by the more general \code{dense_as_general()}, which
      takes arguments controlling memory allocation and the
      \dQuote{kind} of the \code{.geMatrix} result.

      \item New C-level utility \code{DimNames_is_symmetric()}
      with R interface \code{isSymmetricDN()}, which should be
      used consistently to test for symmetry of \code{[dD]imnames}.
      Note that these are intended to behave consistently with
      \code{symmetricMatrix_validate()},
      by allowing, e.g., \code{list(NULL, nms)},
      but not, e.g., \code{list(A = NULL, B = nms)}.

      \item Coercions to \code{triangularMatrix} and
      \code{symmetricMatrix} are now almost all inherited from
      \code{Matrix}, whose methods simply call \code{tri[ul]()}
      and \code{forceSymmetric()} if \code{isTriangular()} and
      \code{isSymmetric()}, respectively, return \code{TRUE}.

      \item Many of the exported \code{.*2*} utilities have been
      redefined as aliases or wrappers of new, more general functions
      (see below).  These not-yet-deprecated functions have been
      centralized in \file{R/denseMatrix.R} and \file{R/sparseMatrix.R}.

      \item New C-level utilities
      \code{R_(dense|sparse)_as_kind()}
      for coercion from one \dQuote{kind} to another;
      \code{R_(dense|sparse)_as_general()}
      for coercion from triangular and symmetric to general;
      \code{R_(dense|sparse)_band()}
      for coercion to triangular (and other banded);
      \code{R_(unpacked*|packed*|sparse)_force_symmetric()}
      for coercion to symmetric;
      \code{R_(dense|sparse)_as_(sparse|dense)()}
      for coercion between dense and sparse of the same \dQuote{kind}
      and \dQuote{structure};
      \code{R_diagonal_as_sparse()}
      for coercion from \code{diagonalMatrix} to any
      \code{[CRT]sparseMatrix};
      \code{R_(dense|sparse|geMatrix)_as_(matrix|vector)()}
      for coercion to base matrix and vector; and
      \code{tCRsparse_as_RCsparse()} for the idioms
      \code{as(t(<[CR]sparseMatrix>), "[RC]sparseMatrix")}.
      These all have not-yet-exported R wrappers.

      \item \code{indTri()} and \code{indDiag()} now in C,
      with a new argument \code{packed} for efficiently indexing
      \code{packedMatrix}.  \code{indDiag()} now behaves sensibly
      in the \code{n = 0} case.

      \item \code{.M.kind()}, \code{.M.shape()}, and (new)
      \code{.M.repr()} are now done in C via \code{R_check_class_etc()},
      requiring a class definition only in \dQuote{rare} cases.
    }
  }
}

\section{Changes in version 1.4-1 (2022-03-21 r3446)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{diag(x)} methods now mostly also keep \code{names} from
      \code{dimnames(x)} by default and obey \code{names=*} more generally.
    }
  }
  \subsection{New Features}{
    \itemize{
      \item New virtual class \code{packedMatrix} containing packed (dense)
      symmetric and triangular matrices.  Methods for subscripting,
      including \code{diag()}, notably keeping names by default and for
      \code{t()} which are memory efficient, i.e., do not work via
      unpacking, thanks to Mikael Jagan.

      \item New \code{dmperm()} implementing a Dulmage-Mendelsohn
      decomposition, thanks to the persistency of Mauricio Vargas (@uc.cl).

      \item Export more low-level conversion utilities:
      \code{.n2dgT}, \code{.m2ngCn}, \code{.m2ngTn}.

      \item Provide some matrix multiplication methods for \code{"RsparseMatrix"}.
    }
  }
  \subsection{Misc}{
    \itemize{
      \item Our C sources now use \code{R_Calloc()}, \code{R_Free()}
      etc, instead of the shorter versions without 'R_'. Consequently,
      we get closer to \code{STRICT_R_HEADERS}.
      Also, include \code{<float.h>} for \code{DBL_EPSILON}.

      \item \code{PROTECT()} 3 more; as from \samp{rchk} for Matrix 1.4-0.

      \item Slightly better patch notably for Windows,
      for \file{src/SuiteSparse_config/SuiteSparse_config.h}

      \item Fix to continue working for \R{} 3.5.0 and newer, providing
      \code{tryInvokeRestart()} for older versions of \R{}, thanking
      Michael Chirico for the heads up.

      \item Modified \code{AZERO()} to work with \code{R_xlen_t} and new
      \code{AZEROs()} for \code{size_t}; other tweaks to evade
      \code{-Wconversion} warnings.
    }
  }
}

\section{Changes in version 1.4-0 (2021-12-08 r3419)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item Update many \file{src/*.c} preventing integer overflow in
      \dQuote{pointer} and index computations for large (dense) matrices;
      prompted by Dario Strbenac's post to R-devel.% June 7, "dgTMatrix Segmentation Fault"

      \item \code{sparse.model.matrix(.., contrasts.arg = <.. ddiMatrix ..>)}
      now works correctly, fixing R-forge Matrix bug #6673 by Davor Josipovic.

      \item \code{sparse.model.matrix(..)} now also works in cases the
      contrast matrix has become a \code{"denseMatrix"}; e.g., in a case
      using \code{poly(.)} in the formula; now works correctly, fixing
      R-forge Matrix bug #6657 and useful suggestions by Nick Hanewinckel.

      \item Fixed the internal \code{attr.all_Mat()} auxiliary for
      \code{all.equal()}, notably for the case when exactly one of the
      matrices is a base \code{matrix}.

      \item Fixed long-standing bug in the \code{rbind2()} method for
      logical dense matrices, specifically \code{"lgeMatrix"}, thanks to
      the notice by Aaron Lun.

      \item fix leak in C-level \code{Csparse_to_dense()} thanks to Bill
      Dunlap in \R{}'s \PR{18204} and \code{install_lu()}
      called from \code{solve(<dgCMatrix>)} in \PR{18206}.

      \item fix leak in crossprod(<dsTMatrix>), thanks to report and patch
      in \PR{18205} by Bill Dunlap.

      \item \code{band(M, k1, k2)} now also works when \code{k1 * k2} is
      larger than 2^31-1, the maximal integer, fixing R-forge Matrix bug
      #6743 by Ariel Paulson.  Further, it works when \code{M} is a sparse
      \code{"symmetricMatrix"} but the band is not symmetric, \code{k1 != -k2}.

      \item fix leak in C-level code for \code{cbind(m1,m2)} or
      \code{rbind(*)} when \code{m1} is \code{"n.Csparse"} and \code{m2} is
      not, thanks to Bill Dunlap's diagnosis and patch in \R{}'s \PR{18210}.

      \item \code{sparseVector(i=integer(), length=2^33)} now does show/print,
      after fixing a bug in the \code{head()} method for \emph{empty}
      sparseVectors.  Reported by David Cortes as Matrix bug #6745.

      \item inverting or solving \code{dsCMatrix} no longer catches C level
      warnings too early which would not free, but rather leak memory;
      thanks to Bill Dunlap's analysis and patch in \R{}'s \PR{18214}.
      Additionally, such warnings and error are \code{message()}d when
      \code{getOption("Matrix.verbose")} is \code{ >= 1}.

      \item \file{test-tools-1.R}: \code{Sys.memGB()} can no longer use
      \code{memory.limit()} on Windows; no returns \code{NA.value = 2.10201}
      in such cases.

      \item \code{ss <-  <sparseVector>[i]} gave an invalid sparseVector
      \code{ss} as \code{ss@i} was not necessarily sorted; thanks to a
      report by Quran Wu.

      \item \code{as(<dsyMatrix>, "generalMatrix")} and similar, sometimes
      did \emph{not} use (C-level) \code{symmetric_Dimnames()} etc; report
      (to \R{}'s \PR{18250} by  Mikael Jagan); fixed all on C level.  As a
      consequence, you will now see \emph{more} preserved dimnames after
      matrix transformations or operations which involved symmetric matrices.

      \item \code{as(<ddiMatrix>, "matrix")} no longer loses dimnames,
      thanks to Mikael Jagan's report as Matrix bug #6751.
    }
  }
  \subsection{Misc}{
    \itemize{
      \item No longer include \file{Rdefines.h} as it is somewhat deprecated.
    }
  }
}

\section{Changes in version 1.3-4 (2021-05-24 r3392)}{
  \subsection{Misc}{
    \itemize{
      \item Update \code{matrix(<data>, n,m)} in \file{tests/*} to work with
      \command{R-devel CMD check --as-cran}
    }
  }
}

\section{Changes in version 1.3-3 (2021-05-01 r3390)}{
  \subsection{Deprecated and Defunct}{
    \itemize{
      \item \code{cBind()} and \code{rBind()} are now defunct:
      simply use \code{cbind()} and \code{rbind()} instead.
    }
  }
  \subsection{Dependency}{
    \itemize{
      \item For now revert to \code{R (>= 3.5.0)} dependency to accomodate
      users on older R installations.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Fixed a thinko (in 1.3-2): Now direct coercion from
      \code{"ddiMatrix"} to \code{"dgCMatrix"}, and hence, e.g.,
      \code{as(Matrix(1, sparse=TRUE), "dgCMatrix") now works.}

      \item Fixed error message in <dtrMatrix> multiplication.

      \item Fixed long-standing bug in \code{R[,j] <- v} when \code{R} is "Rsparse*",
      R-forge Matrix bug #6709 by David Cortes.

      \item \file{./include/cholmod.h} and \file{./include/Matrix_stubs.h}
      needed updating from SparseSuite update; R-forge Matrix bug #6714 by
      Kasper Kristensen (TMB pkg).

      \item \code{as.matrix()} and \code{as.array()} now work for
      \code{"sparseVector"}s as expected; see Matrix bug #6708.

      \item \code{M[,]} (and similar) now work as in base \R{};
      R-forge Matrix bug #6720 by David Cortes.

      \item \code{-S} now works also when \code{S} has no `factors` slot.
      It signalled an error, e.g., for sparse triangular matrices \code{S};
      R-forge Matrix bug #6656, reported by Chun Fung (Jackson) Kwok.

      \item \code{M*2} and similar no longer keep cached factorizations (in
      `factors` slot), but drop them via internal new \code{.empty.factors()}.
      R-forge Matrix bug #6606, reported by Tomas Lumley.

      \item removed the nowhere used (and unexported but still active)
      class union \code{"Mnumeric"} which actually trickled into many base
      classes properties.  Notably would it break validity of \code{factor}
      with a proposed change in validity checking, as factors were also
      \code{"Mnumeric"} but did not fulfill its validity method.
      Similarly removed (disabled) unused class union \code{"numericVector"}.

      \item removed a few duplicated \code{.alias{.}} from \file{man/*.Rd}.
    }
  }
  \subsection{Misc}{
    \itemize{
      \item translation updates (of outlines only); finally added Italian
      (by Daniele Medri) to svn; updated French (by Philippe Grosjean),
      forgotten (R part of) Korean.
      New Lithuanian translations by Gabriele Stupuriene & Rimantas Zakauskas.

      \item In internal \code{diagOdiag()} method, no longer use
      \code{matrix(x, n,n)} when \code{x} is longer than n*n.

      \item Update tests/*.R to future \code{matrix(x, n,k)} warning in more
      mismatch cases.

      \item Eliminating the need for \file{ftp://*}, add the very small
      \code{jgl009} MatrixMarket example to our \file{external/} files.
    }
  }
}

\section{Changes in version 1.3-2 (2021-01-05 r3362)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{rankMatrix()} tweaks for the singular values based methods,
      notably \code{method = "maybeGrad"}.

      \item \code{as(new("dtCMatrix", diag="U"), "matrix")} now works, as
      C-level \code{diagU2N()} now also works for 0-dimensional triangular
      matrices; this also fixes a subsetting (\dQuote{indexing}) bug of
      such 0-dimensional matrices, thanks to a report by Aaron Lun.

      \item logical subsetting of 0-dim. (diagonal/triangular) matrices
      fixes.

      \item The new \code{FCONE} macros added for newer Fortran/C compiler
      combinations are now defined back compatibly with R >= 3.6.0.

      \item \code{chol(<diagonal dt[CRT]Matrix>)} now works.

      \item \file{rchk}: fix potentially un\code{PROTECT}ed vars in
      \file{src/dsyMatrix.c}
    }
  }
}

\section{Changes in version 1.3-1 (2020-12-23 r3352)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{rankMatrix(<dense>, method="qr.R")} no longer assumes
      non-negative diagonal entries of the \eqn{R} matrix.
    }
  }
}

\section{Changes in version 1.3-0 (2020-12-15 r3351)}{
  \subsection{Significant User-Visible Change}{
    \itemize{
      \item \code{Matrix(*, doDiag=TRUE)} where \code{doDiag=TRUE} has
      always been the \emph{default} is now obeyed also in the sparse
      case, as all \code{"diagonalMatrix"} are also \code{"sparseMatrix"}.

      \code{Matrix(0, 3,3)} returns a \code{"ddiMatrix"} instead of a
      \code{"dsCMatrix"} previously.  The latter is still returned from
      \code{Matrix(0, 3,3, doDiag=FALSE)}, and e.g., \code{.symDiagonal(3,pi)}.

      Also a triangular matrix, e.g., \code{"dtrMatrix"} is detected now in
      cases with \code{NA}s.

      This is both a bug fix \emph{and} an API change which breaks code
      that assumes \code{Matrix(.)} to return a \code{"CsparseMatrix"} in
      cases where it now returns a \code{"diagonalMatrix"} (which does
      extend \code{"sparseMatrix"}).
    }
  }
  \subsection{New Features}{
    \itemize{
      \item Subassignment to \code{"diagonalMatrix"} now returns
      sparse \code{"triangularMatrix"} more often; also (sparse)
      \code{"symmetricMatrix"}.

      \item \code{nearPD()} gets new option: If \code{base.matrix = TRUE},
      the resulting \code{mat} component is a \pkg{base} \code{matrix}, as
      often used desired when \code{nearPD()} is used outside the
      \pkg{Matrix} package context.

      \item Factored out new \code{qr2rankMatrix()} utility from
      \code{rankMatrix()}.

      \item New \code{BunchKaufman(<matrix>)} method.

      \item Added \code{wrld_1deg} sparse matrix example to
      \emph{distributed} version of \pkg{Matrix} (no longer excluding it
      via \file{.Rbuildignore}).

      \item New (simple) \code{mat2triplet()} function to be used instead
      of \code{summary(<sparseMatrix>)} in code.

      \item Internal \code{.diag2tT()} gains new option \code{drop0 = TRUE}
      and hence now by default drops zero diagonal entries.  Consequently,
      e.g., \code{as(<diagonalMatrix>, "CsparseMatrix")} now drops such
      zeros, too.

      \item Updated the crucial underlying C libraries from SuiteSparse,
      from 4.2.1 to 5.7.1 (from 2020-02-20), visible in \code{.SuiteSparse_version()} .

      \item \code{sparseMatrix()} gets new argument \code{repr = "C"},
      superseding the (now deprecated) \code{giveCsparse = TRUE}.  Allows
      to return \code{"RsparseMatrix"} matrices.

      Similarly, \code{rsparsematrix()}, \code{fac2sparse()} and \code{fac2Sparse()}
      get the new \code{repr} argument and their \code{giveCsparse} is
      deprecated, sometimes only informally for now.% no deprecation warning yet

      \item \code{sparse.model.matrix()} gets option \code{sep = ""}, with,
      e.g., \code{sep = ":"} allowing to get easier column names; from
      R-forge Matrix (non-)bug #6581, by Vitalie Spinu.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{rankMatrix(<sparse>, method="qr")} now returns \code{NA}
      (or \code{NaN}) instead of signalling an error in the case the sparse
      \eqn{Q R} decomposition gave \code{NA}s in \code{diag(R)}.

      \item Coercion (\code{as(., .)}) from e.g., \code{"lsyMatrix"} to
      \code{"CsparseMatrix"} silently made asymmetric dimnames symmetric,
      as did the \emph{internal} \code{forceCspSymmetric(*, dimNames)}
      which may be called from \code{forceSymmetric()}.

      \item Adapt code to new Fortran requirements of passing length of
      character arguments, thanks to Brian Ripley.

      \item (R-forge Matrix bug #6659, reported by Georg Kindermann):
      \code{<sparseVector>[i] <- val} bug fixed.

      \item (R-forge Matrix bug #6666, reported by Ezra Tucker):
      \code{which(<ldiMatrix>, array.ind=TRUE)} thinko fixed.

      \item For R-devel Dec 4, 2020: adapt all.equal() check of sparse
      matrix images (which contain panel functions with environments ..).

      \item tried fixing warning \emph{'cholmod_factorize_p' accessing 16
	bytes in a region of size 8 [-Wstringop-overflow=]} in
      \file{src/dsCMatrix.c} (in two places); seen by pre-release-gcc11 compilation.
    }
  }
}

%
\section{Changes in version 1.2-18 (2019-11-26, manually picked from svn)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item Fix last(?) \code{PROTECT()} warning found by \command{rchk} in
      \file{src/dense.c}'s \code{ddense_skewpart()}.

      \item \code{as(m, "dgTMatrix")} does not lose \code{dimnames} anymore
      when \code{m} is a (traditional) \code{matrix}.

      \item \code{M[logical(0), ]} now has dimension \eqn{0 x k} for sparse
      \code{M} as for base matrices.

      \item \code{log(M, base)} (the 2-argument version of \code{log()})
      wrongly gave the result for \code{base = exp(1)}, i.e., the 1-argument default.

      \item \file{test-tools-Matrix.R}: \code{Qidentical()} no longer assumes
      \code{class(<matrix>)} to be of length 1.

      \item \file{test-tools-1.R}: provide bug-fixed \code{canCoerce()} in
      old R versions.
    }
  }
}

\section{Changes in version 1.2-17 (2019-03-20, svn r3294)}{
  \subsection{New Features}{
    \itemize{
      \item (none)
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Fix new \code{PROTECT()} warnings (bugs?) found by \command{rchk}.

      \item Provide \code{isFALSE()} for \R{} < 3.5.0 as now need it for
      sparseMatrix printing.
    }
  }
}

\section{Changes in version 1.2-16 (2019-03-04, svn r3291)}{
  \subsection{New Features}{
    \itemize{
      \item regression tests depending on \code{sample()} now are future
      proof reproducible, via \code{RNGversion(.)}.

      \item give information about #{rows} and #{columns} that are
      suppressed in print()ing if the matrix is larger than `max.print`.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{data(<Matrix-data>)} no longer attaches \pkg{Matrix} to
      the search path.

      \item \code{"Ops"} group methods, i.e., "Arith", "Compare" & "Logic",
      now should all work with 0-extent matrices as well, thanks to bug
      reports by Aaron Lun, University of Cambridge.

      \item when printing and formatting sparse matrices, see \code{formatSpMatrix()},
       the \code{maxp} option, e.g., from \code{getOption("max.print")}, is
      \dQuote{rounded up} to 100, as very small values are very rarely appropriate.
    }
  }
}

\section{Changes in version 1.2-15 (2018-08-20, svn r3283)}{
  \subsection{New Features}{
    \itemize{
      \item \code{image()} gets new optional argument \code{border.color}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{image(Matrix(0, n,m))} now works.
    }
  }
}


\section{Changes in version 1.2-14 (2018-04-08, svn r3278)}{
  \subsection{New Features}{
    \itemize{
      \item German translation updates.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item one more \code{PROTECT()}.
    }
  }
}

\section{Changes in version 1.2-13 (2018-03-25, svn r3275)}{
  \subsection{New Features}{
    \itemize{
      \item Faster \code{as(<matrix>, "sparseMatrix")} and coercion
	\code{"dgCMatrix"}, \code{"ngCMatrix"}, etc, via new direct C
	\code{matrix_to_Csparse()} which does \emph{not} go via
	\code{"dgeMatrix"}.  This also works for large matrices
	\code{m}, i.e., when \code{length(m) >= .Machine$integer.max}.

	Also provide low-level \R{} functions \code{.m2dgC()},
	\code{.m2lgC()}, and \code{.m2ngC()} for these.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{cbind(NULL,<Matrix>)} no longer return \code{NULL};
      analogously for \code{rbind()}, \code{rbind2()}, \code{cbind2()},
      fixing very long standing typo in the corresponsing \code{cbind2()}
      and \code{rbind2()} methods.

      \item The deprecation warning (once per session) for
      \code{cBind()} and \code{rBind()} finally works (fixing a simple thinko).

      \item \code{cbind()} and \code{rbind()} for largish sparse
      matrices no longer gives an error because of integer overflow
      (in the default case where \code{sparse} is not been specified
      hence is chosen by a \code{nnzero()} based heuristic).

      \item \code{.symDiagonal(5, 5:1)} and \code{.trDiagonal(x = 4:1)}
      now work as expected.

      \item \code{Sp[i]} now is much more efficient for large sparse
      matrices \code{Sp}, notably when the result is short.

      \item \code{<sparseVector>[ <negative integer> ]} now also gives
      the correct answer when the result is \dQuote{empty}, i.e., all
      zero or false.

      \item large \code{"dspMatrix"} and \code{"dtpMatrix"} objects can
      now be constructed via \code{new(*, Dim = *, x = *)} also when
      \code{length(x)} is larger than 2^31 (as the C internal
      validation method no longer suffers from integer overflow).

      \item More \samp{PROTECT()}ing to be \dQuote{rather safe than
	sorry} thanks to Tomas Kalibera's check tools.
    }
  }
}

\section{Changes in version 1.2-12 (2017-11-10, svn r3239)}{
  \subsection{New Features}{
    \itemize{
      \item \code{crossprod(x,y)} and \code{kronecker(x,y)} have become
      considerably more efficient for large \code{"indMatrix"} objects
      \code{x, y}, thanks to private nudging by Boris Vaillant.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item (R-forge Matrix bug #6185): \code{c < 0} now also works for
      derived sparse Matrices (which only \emph{contain} Matrix
      classes); via improving hidden \code{MatrixClass()}.   Part of
      such derived matrices only work in R >= 3.5.0.

      \item using \code{Authors@R} in \file{../DESCRIPTION} to list all
      contributors.

      \item \code{solve(-m)} no longer should use a cached Cholesky
      factorization (of \code{m}).
    }
  }
}

\section{Changes in version 1.2-11 (2017-08-10, svn r3225)}{
  \subsection{New Features}{
    \itemize{
      \item S4 method dispatch no longer emits ambiguity
      notes (by default) for everybody, apart from the package
      maintainer.  You can reactivate them by
      \code{options(Matrix.ambiguityNotes = TRUE)}
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{rankMatrix(<matrix of all 0>)} now gives zero for all
      methods, as it should be.

      \item no longer calling \code{length(NULL) <- <positive>} which
      has been deprecated in R-devel since July.

      \item \code{qr.coef(<sparseQR>, y)} now finally has correct (row)
      names (from pivot back permutation).

      \item \code{.trDiagonal()} utility is now exported.
    }
  }
}

\section{Changes in version 1.2-10 (2017-04-19, svn r3216)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item quite a collection of new \code{PROTECT(.)}'s thanks to
      Tomas Kalibera's \sQuote{rprotect} analysis.
    }
  }
}

\section{Changes in version 1.2-9 (2017-03-08, svn r3211)}{
  \subsection{New Features}{
    \itemize{
      \item \code{"Ops"} between "table", "xtabs", and our matrices now work.
      \item \code{as(matrix(diag(3), 3, dimnames=rep(list(c("A","b","c")),2)),
 	"diagonalMatrix")@x} is no longer named.
      \item \code{norm(x, "2")} now works as well (and equivalently to \code{base::norm}).
      \item \code{sparseVector()} now also works without \code{x} argument.
      \item \code{c.sparseVector()} method for \code{c()} of
      sparseVectors (and available as regular function on purpose).
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{as(Diagonal(3), "denseMatrix")} no longer returns a
      non-dense \code{"ddiMatrix"}.

      \item \code{S[sel,] <- value} and similar no longer segfault, but
      give a \code{"not (yet?) supported"} error
      for sparse matrices \code{S} and logical \code{sel} when
      \code{sel} contains \code{NA}s.

      The same error (instead of a low-level one) is signalled for
      \emph{indexing} (with NA-containing logical \code{sel}), i.e.,
      \code{S[sel,]}.
      %% from in ../TODO :
      %% \item \code{S[sel,]}, \code{S[,sel] <- value} and similar now also work for
      %% sparse matrices \code{S} and logical \code{sel} when \code{sel} contains \code{NA}s.

      \item \code{which(x, arr.ind=TRUE, *)} (when \code{x} is a
      \code{"lMatrix"} or \code{"nMatrix"}) now works the same as
      \code{base::which}, obeying an optional \code{useNames} argument
      which defaults to \code{TRUE}.  Previously, the resulting
      two-column matrix typically had empty \code{dimnames}.
    }
  }
}

\section{Changes in version 1.2-8 (2017-01-16, svn r3201)}{
  \subsection{New Features}{
    \itemize{
      \item 0-length matrix \code{"Ops"} (binary operations) are now
      compatible to R-devel (to be \R{} 3.4.0).
      \item C-API: \code{SuiteSparse_long} is now defined as
      \code{int64_t} on all platforms, and we now include (C99) \file{inttypes.h}
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{x[.] <- value} now also works for
      \code{"sparseVector"}'s, both as \code{x} and as \code{value}.
      \item \code{x[FALSE] <- value} now also works for \code{"sparseVector"}'s.

      \item \code{rep(x, *)} now works for \code{"sparseVector"}s and
      sparse and dense \code{"Matrix"}-classed matrices \code{x}.

      \item \code{solve(<sparse_LU>)} no gives an error in some cases of
      singular matrices, where before the C code accessed illegal memory locations.
    }
  }
}


\section{Changes in version 1.2-7.1 (2016-08-29, svn r3187)}{
  \itemize{ \item in C code, protect _POSIX_C_SOURCE by #ifdef __GLIBC__ }
}
\section{Changes in version 1.2-7 (2016-08-27, svn r3185)}{
  \subsection{New Features}{
    \itemize{
      \item \code{cBind()} and \code{rBind()} have been almost silently
      deprecated in \R{} \code{>= 3.2.0} and now give a warning,
      \dQuote{once per session} only.

      \item \code{bandSparse(*, k=k, *)} now returns matrices inheriting from
      \code{"triangularMatrix"} when obvious from the diagonal indices \code{k}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{KhatriRao(X,Y)} now also works when \code{X} or
      \code{Y} is completely zero.
    }
  }
}

\section{Changes in version 1.2-6 (2016-04-27, svn r3175)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item The 0-dim. Matrix multiplication fix in 1.2-5 did trigger
      wrong warnings in other diagonal matrix multiplications.
    }
  }
}

\section{Changes in version 1.2-5 (2016-04-14, svn r3170)}{
  \subsection{New Features}{
    \itemize{
      \item \code{isSymmetric(m)} now also works for \code{"indMatrix"} \code{m}.
      \item \code{isSymmetric(m)} is faster for large dense asymmetric matrices.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Matrix multiplications (\code{A \%*\% B}) now work correctly
      when one of the matrices is diagonal and the other has a zero dimension.
    }
  }
}

\section{Changes in version 1.2-4 (2016-02-29, svn r3162)}{
  \subsection{New Features}{
    \itemize{
      \item \code{sparseMatrix()} gets new argument \code{triangular}
      and a smarter default for \code{dims} when \code{symmetric} or
      \code{triangular} is true.
      \item \code{as(<sparse>, "denseMatrix")} now works in more cases
      when \code{prod(dim(.))} is larger than \eqn{2^{31} - 1}.
      Hence, e.g., \code{!S} now works for much larger sparse matrices
      \code{S}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item creating very large dense matrices, e.g., by
      \code{as(<sparseM.>, "matrix")} would segfault (in case it could
      allocate enough storage).
    }
  }
}

\section{Changes in version 1.2-3 (2015-11-19, svn r3155)}{
  \subsection{New Features}{
    \itemize{
      \item \code{MatrixClass()} is exported now.
      \item More exports of semi-internal functions (for speed, named
      \code{".<foo>"}, i.e., inofficial API), such as \code{.solve.dgC.lu()}.
      \item more Korean translations
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Packages \emph{linking} to \pkg{Matrix} (\code{LinkingTo:}
      in \file{DESCRIPTION}) now find
      \samp{alloca} properly defined in \file{Matrix.h} even for non-GNU
      compilation environments such as on Solaris or AIX.

      \item extended "n?CMatrix" classes (e.g., from \code{setClass(.,
	contains="ngCMatrix")}) now can be coerced via \code{as(.)} to
      \code{"d.CMatrix"}.

      \item The printing of largish sparse matrices is improved, notably
      in the case where columns are suppressed, via new \code{fitWidth =
	TRUE} option in \code{printSpMatrix2()}. %%% FIXME __ EXAMPLES __

      \item \code{cbind2()} and \code{rbind2()} no longer fail to
      determine \code{sparse} when it is unspecified and hence
      \code{NA}, fixing R-forge bug #6259.
    }
  }
}

\section{Changes in version 1.2-2 (2015-07-03, svn r3131)}{
  \subsection{New Features}{
    \itemize{
      \item Explicitly import from \dQuote{base} packages such as \code{"stats"}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Our \code{colSums(x)}, \code{rowMeans(y)}, \dots, methods
      now \dQuote{keep names}, i.e., if the result is a numeric vector,
      and the matrix \code{x} has column or row names, these become the
      \code{names(.)} of the result, fixing R-forge bug #6018.
    }
  }
}

\section{Changes in version 1.2-1 (2015-05-30, svn r3127)}{
  \subsection{New Features}{
    \itemize{
      \item \code{"Matrix"} now has an \code{initialization()} method
      coercing 0-length dimnames components to \code{NULL} and other
      non-\code{NULL} dimnames to \code{character}.  Before, e.g.,
      numeric dimnames components partially worked, even though it has
      always been documented that non-\code{NULL} dimnames should be
      \code{character}.
      \item For \code{symmetricMatrix} objects which have symmetrical
      dimnames by definition, it is allowed to only set one half of the
      \code{dimnames} to save storage, e.g., \code{list(NULL, nms)} is
      \emph{semantically} equivalent to \code{list(nms, nms)}.

      \item \code{as.vector(<sparseVector>)} etc, now work, too.
      \item \code{lu(\emph{<sparseMatrix>})} now keeps \code{dimnames}.
      \item better \file{NEWS.Rd} (which pleases Kurt and \command{tidy} ;-)
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{S[] <- T} and \code{S[] <- spV} now work (in more cases)
      for sparse matrices S, T and sparseVector \code{spV}.
      \item Huge dense matrix multiplication did lead to segfaults, see
      R-help, \dQuote{does segfault mean (always) a bug?}, May 5, 2015.
      Fixed by using C's Alloca() only in smallish cases.
      \item Optional arguments in \code{image()}, e.g., \code{main=
	<..>)} now also work for \code{lgCMatrix}, \code{nMatrix} etc;
      thanks to a 4.5 years old report by Mstislav Elagin.
      \item \code{dimnames(A) <- val} now resets the \code{factors} slot
      to empty, as the factorizations now keep dimnames more often.
      \item \code{crossprod(<matrix>, Diagonal(<n>))} works again (and
      these are tested more systematically).
      \item Matrix products (\code{\%*\%}, \code{crossprod}, and
      \code{tcrossprod}) for \code{"dtrMatrix"} are correct in all
      cases, including keeping dimnames.
      \item \code{Matrix(d)} (and other coercions to \code{"Matrix"})
      now correctly keeps \code{dimnames} also when \code{d} is a
      traditional \emph{diagonal} \code{"matrix"}.
    }
  }
}

\section{Changes in version 1.2-0 (2015-04-03, svn r3096)}{
  \subsection{New Features}{
    \itemize{
      \item New \code{\%&\%} for \dQuote{boolean arithmetic} matrix product.
      \item New argument \code{boolArith = NA} in \code{crossprod()} and
      \code{tcrossprod()}.  \code{boolArith = TRUE} now forces boolean
      arithmetic, where \code{boolArith = FALSE} forces numeric one.

      Several of these products are more efficient thanks to new C
      functionality based on our new \code{chm_transpose_dense()}, and
      others based on \code{geMatrix_crossprod},
      \code{geMatrix_matrix_mm}, etc.

      \item Most dense matrix products, also for non-\code{dgeMatrix},
      including \code{"l..Matrix"} and \code{"n..Matrix"} ones are now
      directly handled by new \code{.Call()}s.

      \item \code{"dMatrix"} (numeric) and \code{"lMatrix"} (logical)
      matrices can now be coerced to \code{"nMatrix"} (non-zero pattern
      or \dQuote{boolean}) even when they contain \code{NA}s, which then
      become \code{TRUE}s.

      \item More thorough checking of \code{cbind2()} and
      \code{rbind2()} methods, notably as they are called from \code{cbind()}
      and \code{rbind()} from \R{} version 3.2.0 on.

      \code{rbind2(<dense>, <dense>)} is faster, being based on new C code.

      \item symmetric Matrices (i.e., inheriting from
      \code{"symmetricMatrix"}) are allowed to have \code{dimnames} of
      the form \code{list(NULL, <names>)} \emph{and} now print correctly
      and get correctly coerced to general matrices.

      \item \code{indMatrix} object (\dQuote{index matrices}) no longer
      need to be \dQuote{skinny}.

      \item \code{rsparseMatrix()} now accepts \code{rand.x = NULL} and
      then creates a random \emph{patter\bold{n}} matrix
      (\code{"nsparseMatrix"}).

      \item \code{anyDuplicatedT()} and \code{uniqTsparse()} low level
      utilities are exported now.

      \item Partial Korean translations of messages.
    }
  }
  \subsection{Deprecation}{
    \itemize{
      \item For \eqn{R \ge 3.2.0}, \code{cBind()} and \code{rBind()}
      are deprecated, as they are no longer needed since \code{cbind()}
      and \code{rbind()} do work automatically.
    }
  }

  \subsection{Bug Fixes}{
    \itemize{
      \item Fix some \code{rbind2()} methods.
      \item \code{t()} now transposes the dimnames even for symmetric
      matrices.
      \item \code{diag(M) <- val} did not always recycle \code{val} to
      full length, e.g., when \code{M} was a \code{"dtrMatrix"}.
      \item \code{crossprod(<indMatrix>)} was wrong in cases where the
      matrix had all-zero columns.
      \item Matrix products (\code{\%*\%}, \code{crossprod}, and
      \code{tcrossprod}) with one sparse and one dense argument now
      return \emph{numeric} (a \code{"dMatrix"}) when they should, i.e.,
      unless the new setting \code{boolArith = TRUE} is applied.
    }
  }
}

\section{Changes in version 1.1-5 (2015-01-18, svn r3037)}{
  \subsection{New Features}{
    \itemize{
      \item More use of \code{anyNA()} (for speedup).
      \item Matrix products (\code{\%*\%}, \code{crossprod},
      \code{tcrossprod}) now behave compatibly to \R{} 3.2.0, i.e., more
      lenient in matching dimensions for matrix - vector products.
      \item \code{isTriangular()} gets new optional argument \code{upper = NA}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{crossprod()} and \code{tcrossprod()} fixes for
      several <diagonal> o <sparse> combinations.
      \item \code{rowMeans(<dgeMatrix>, na.rm=TRUE)} was wrong sometimes.
      \item fix and speedup of coercions (\code{as(., .)}) from and to
      symmetric or triangular matrices.
      \item \code{invPerm()} coercion to integer
      \item \code{dimnames( solve(.,.) )} fix [r3036]
      \item \code{tril()} and \code{triu()} now return correct \code{uplo}.
      \item \code{names(dimnames(.))} now preserved, e.g. in
      \code{symmpart()} or subsetting (\code{A[i,j]}).
    }
  }
}

\section{Changes in version 1.1-4 (2014-06-14, svn r2994)}{
  \subsection{New Features}{
    \itemize{
      \item new \code{rsparsematrix()} for random sparse Matrices.
      \item improved warnings, notably for unused arguments previously
      swallowed into \code{...}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{crossprod(<vec>, <dsyMatrix>)} fixed.
      \item \code{crossprod()} and \code{kronecker()} fixes for some
      <indMatrix> cases.
    }
  }
}

\section{Changes in version 1.1-3 (2014-03-30, svn r2982)}{
  \subsection{New Features}{
    \itemize{
      \item \code{\%*\%} and \code{crossprod()} now also work with
      \code{sparseVector}s.
      \item speedup of \code{crossprod(v, <sparseM>)}, thanks to nudge
      by Niels Richard Hansen.
      \item new help page for all such matrix products
      (\file{../man/matrix-products.Rd}).
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{image()} now gets correct \code{ylim} again.
      \item More consistent matrix products.
    }
  }
}

\section{Changes in version 1.1-2-2 (2014-03-04, svn r2966)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item correct adaption to \R{} 3.1.0
      \item using \code{tolerance} (and not \sQuote{tol}) in \code{all.equal()}
    }
  }
}

\section{Changes in version 1.1-2 (2014-01-28, svn r2962)}{
  \subsection{New Features}{
    \itemize{
      \item export fast power-user coercion utilities
      \code{.dsy2mat()}, \code{.dxC2mat()}, \code{.T2Cmat()}, \code{..2dge()}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item matrix products now (mostly) work with \code{sparseVector}s;
      and correctly in some more cases.
    }
  }
}

\section{Changes in version 1.1-1.1 (2013-12-30, svn r2957)}{
    \itemize{
      \item Testing code's \code{assertWarning()} adapted for \eqn{R \le 3.0.1}.
      \item \code{Depends: R >= 2.15.2} eases checking.
    }
}

\section{Changes in version 1.1-1 (2013-12-28)}{
  \subsection{New Features}{
    \itemize{
      \item \code{image(.., xlim, ylim)}: nicer defaults %% ../R/dgTMatrix.R
      for the axis limits, and \code{ylim} is sorted decreasingly; not
      strictly back-compatible but should never harm.
      \item \code{rankMatrix(*, method="qr")} now using \code{tol}
      \item \code{T2graph()} and \code{graph2T()} export old functionality explicitly.
      Tweaks in conversions between \code{"graph"} and
      \code{"sparseMatrix"} objects.  Notably, \code{as(<graph>,
	<Matrix>)} now more often returns a (0/1 pattern) "n..Matrix".
      \item \code{sparseMatrix()}: new \code{use.last.ij} argument.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{KhatriRao()}: fix rownames (X <-> Y)
      \item \code{qr.coef()}, \code{qr.fitted}, and \code{qr.resid} now
      also work with \emph{sparse} RHS \code{y}.
      \item sparse matrix \dQuote{sub assignments}, e.g., \code{M[ii] <- v},
      speedup and fixes.
      \item bug fixes also in \code{M[negative indices] <- value} and
      \code{<sparseMatrix>[cbind(i,j)]}.
    }
  }
}

\section{Changes in version 1.1-0 (2013-10-21, svn r2930)}{
  \subsection{New Features}{
    \itemize{
      \item \code{fac2sparse} and \code{fac2Sparse} now exported, with a
      new \code{giveCsparse} option.
      \item Update to latest \command{SuiteSparse} C library by Tim Davis,
      U. Florida.
      \item ensuing \dQuote{C API changes}
      \item new \code{.SuiteSparse_version()} function
      \item Many \sQuote{Imports:} instead of \sQuote{Depends:}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item fixed long lasting undetected \code{solve(<dsCMatrix>, *)}
      bug.
      \item Our \code{all.equal()} methods no longer sometimes return
      \code{c("TRUE", "....difference..")}.
      \item \code{rankMatrix(<matrix>)}: fix the internal \code{x.dense}
      definition.
    }
  }
}

\section{Changes in version 1.0-14 (2013-09-12, svn r2907)}{
  \subsection{Bug Fixes}{
    \itemize{
      \item Revert some wrong changes to \code{solve(<sparse>, *)} from
      1.0-13 (\dQuote{stop gap fix} for \R{} 3.0.2).
    }
  }
}

\section{Changes in version 1.0-13 (2013-09-10, svn r2904)}{
  \subsection{New Features}{
    \itemize{
      \item New (efficient) \code{KhatriRao()} function by Michael Cysouw
      \item New \code{"indMatrix"} class of \dQuote{index matrices}, a
      generalization of \code{"pMatrix"}, the permutation matrices, many
      methods generalized from pMatrix to indMatrix.  All (initial)
      functionality contributed by Fabian Scheibl, Univ.\sspace{} Munich.
      \item Export and document \code{isDiagonal()} and
      \code{isTriangular()} as they are useful outside of \pkg{Matrix}.
      \item \code{rankMatrix(M, method="qr")} no longer needs
      \code{sval} which makes it considerably more useful for large
      sparse \code{M}.
      \item Start providing \code{anyNA} methods for \eqn{R >= 3.1.0}.
      \item \code{solve(<sparse> a, <sparse> b)}: if \code{a} is
      symmetric, now compute \emph{sparse} result.
      \item \code{nearPD()} gets new option \code{conv.norm.type = "I"}.
      \item \code{determinant(<dpoMatrix>)} now uses \code{chol()}, and
      hence also an existing (\sQuote{cached}) Cholesky factor.
      \item 3 new \code{C -> R} utilities (including hidden \R{} function
      \code{.set.factors()} for caching also from \R{}, not just in C).
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{M[] <- v} for unitriangular \code{M} now correct.
      \item \code{lu(.)} no longer sometimes returns unsorted columns.
    }
  }
}
%% TODO: fill in from here <<<<<<<<<<<<<<<<<<<
%% ----  using ../ChangeLog and notably my ../svn-log-from.all
\section{Changes in version 1.0-12 (2013-03-26, svn r2872)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}


\section{Changes in version 1.0-11 (2013-02-02)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{as(<csr>, "dgCMatrix")} (from package \CRANpkg{SparseM})
      now works again.
      \item .
    }
  }
}

\section{Changes in version 1.0-10 (2012-10-22)}{
  \subsection{New Features}{
    \itemize{
      \item \code{.sparseDiagonal()}: new \code{unitri} argument, and
      more flexibility;
      \item new \code{solve(<dsCMatrix>, <missing>)} via efficient C code.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}

\section{Changes in version 1.0-9 (2012-09-05)}{
  \subsection{New Features}{
    \itemize{
      \item new \code{sparseVector()} constructor function.
      \item \code{is.finite()} \code{is.infinite()} now work for our
      matrices and "*sparseVector" objects.
      \item \code{diag(.) <- V} now preserves symmetricity,
      triangularity and even uni-triangularity sometimes.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Quite a few fixes for \code{Ops} (arithmetic, logic, etc)
      group methods.
      \item Ditto for \code{diagonalMatrix} methods.
    }
  }
}

\section{Changes in version 1.0-6 (2012-03-16, publ. 2012-06-18)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}

\section{Changes in version 1.0-5 (2012-03-15)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}

\section{Changes in version 1.0-4 (2012-02-21)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}

\section{Changes in version 1.0-3 (2012-01-13)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}

\section{Changes in version 1.0-2 (2011-11-19)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}

\section{Changes in version 1.0-1 (2011-10-18)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}

\section{Changes in version 1.0-0 (2011-10-04)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}

\section{Changes in version 0.9996875-3 (2011-08-13)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}

\section{Changes in version 0.9996875-2 (2011-08-09)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}

\section{Changes in version 0.9996875-1 (2011-08-08)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }

  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}

\section{Changes in version 0.999375-50 (2011-04-08)}{
  \subsection{New Features}{
    \itemize{
      \item .
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}


% How can I add vertical space ?

% \preformatted{} is not allowed, nor is \cr





%--------------- start of DB+MM history: ------------------------

\section{Changes in version 0.95-1 (2005-02-18, svn r561)}{
  \subsection{Authorship}{
    \itemize{
      \item During Doug Bates' sabbatical in Zurich,
      Martin Maechler becomes co-author of the \pkg{Matrix} package.
    }
  }
  \subsection{New Features}{
    \itemize{
      \item Beginning of class reorganization with a more systematic
      naming scheme.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item More (correct) coercions \code{as(<from>, <to>)}.
    }
  }
}

\section{Changes in version 0.9-1 (2005-01-24, svn r451)}{
  \subsection{New Features}{
    \itemize{
      \item lme4 / lmer specific R code moved out to \CRANpkg{lme4} package.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item .
    }
  }
}




% How can I add vertical space ?  ( \preformatted{} is not allowed, nor is \cr )


%--------------- pre-pre-history: ------------------------

\section{Changes in version 0.8-2 (2004-04-06, svn r51)}{
  \subsection{Authorship}{
    \itemize{
      \item Doug Bates (only)
    }
  }
  \subsection{New Features}{
    \itemize{
      \item Sparse matrices, classes and methods, partly via
      \item Interface to LDL, TAUCS, Metis and UMFPACK C libraries
    }
  }
}

% How can I add vertical space ? .................................


\section{Version 0.2-4}{
\subsection{..., 0.3-1, 0.3-n (n=3,5,...,26): 22 more CRAN releases}{
  \itemize{ \item ............................................. } }}

% How can I add vertical space ?
% \preformatted{} is not allowed, nor is \cr



\section{Version 0.2-1 (2000-07-15)}{
  The first CRAN release of the \pkg{Matrix} package,
  titled \dQuote{A Matrix library for R} authored by
  Douglas Bates (maintainer, principal author) and Saikat DebRoy.
  \subsection{Features}{
    \itemize{
      \item \code{Matrix()} constructor for \R{} objects of class \code{Matrix}.
      \item \code{Matrix.class()} returning informal subclasses such as
      \code{"Hermitian"}, \code{"LowerTriangular"}
      \item \code{is.Orthonormal()}, \code{is.Hermitian()} ,
      \code{is.UpperTriangular()} functions.
      \item \code{SVD()}, \code{lu()}, and \code{schur()} decomposition
      generics with \code{"Matrix"} methods.
      \item \code{rcond()}, \code{norm()}, \code{det()};
      \code{expand()} and \code{facmul()}.

      \item C++ interface to LAPACK
    }
  }
}
