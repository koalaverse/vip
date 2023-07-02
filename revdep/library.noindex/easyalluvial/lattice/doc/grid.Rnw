\documentclass[a4paper]{article}

\usepackage{Rd}

% \VignetteIndexEntry{Integation with grid}
% \VignettePackage{lattice}
% \VignetteDepends{grid}

% Definitions
\newcommand{\slan}{{\sffamily S}}
\newcommand{\rlan}{{\sffamily R}}
\newcommand{\grid}{\pkg{grid}}
\newcommand{\lattice}{\CRANpkg{lattice}}

\setlength{\parindent}{0in}
\setlength{\parskip}{.1in}
\setlength{\textwidth}{140mm}
\setlength{\oddsidemargin}{10mm}

\title{\lattice{} and \grid{}}
\author{Paul Murrell}

\begin{document}

\maketitle

The \lattice{} package is built on top of \grid{} and provides a
quite sophisticated example of writing high-level plotting functions
using \grid{}.  Because \lattice{} consists of \grid{} calls, it is
possible to both add \grid{} output to \lattice{} output, and
\lattice{} output to \grid{} output.

<<>>=
library(grid)
@ 

\subsection*{Adding \grid{} to \lattice{}}

Panel functions in \lattice{} can include \grid{} calls.
The following example adds a horizontal line at 0 to a standard
\code{xyplot} (see Figure \ref{figure:trellispanel}):

<<echo=FALSE, results=hide>>=
library(lattice)
<<trellisdata, echo=FALSE, eval=FALSE>>=
x <- rnorm(100)
y <- rnorm(100)
g <- sample(1:8, 100, replace = TRUE)
@
% This is the code the reader sees
% This WILL get run by checking code, but by then lattice will be
% installed so it will be ok
<<trellispanelplot, eval=FALSE>>=
xyplot(y ~ x | g, panel = function(x, y) {
    panel.xyplot(x, y);
    grid.lines(unit(c(0, 1), "npc"), unit(0, "native"),
               gp = gpar(col = "grey"))
})
@
% This generates the actual plot
<<trellispanel, echo=FALSE, results=hide, fig=TRUE, width=6, height=6, include=FALSE>>=
<<trellisdata>>
<<trellispanelplot>>
@
\begin{figure}[p]
\begin{center}
{
\includegraphics[width=3.5in, height=3.5in]{grid-trellispanel}
}
\end{center}
\caption{\label{figure:trellispanel}
        A \lattice{} panel function using \grid{}.}
\end{figure}
@
The following  example writes a left-justified label in each strip
(see Figure \ref{figure:trellisstrip}):

<<trellisstripplot, eval=FALSE>>=
xyplot(y ~ x | g, strip = function(which.given, which.panel, ...) {
    grid.rect()
    grid.text(paste("Variable ", which.given, ": Level ",
                    which.panel[which.given], sep = ""),
              unit(1, "mm"), .5, just = "left")
})
<<trellisstrip, echo=FALSE, results=hide, fig=TRUE, width=6, height=6, include=FALSE>>=
<<trellisdata>>
<<trellisstripplot>>
@
\begin{figure}[p]
\begin{center}
{
\includegraphics[width=3.5in, height=3.5in]{grid-trellisstrip}
}
\end{center}
\caption{\label{figure:trellisstrip}
        A \lattice{} strip function using \grid{}.}
\end{figure}

\subsection*{Adding \lattice{} to \grid{}}

It is also possible to use a \lattice{} plot as an element of
a \grid{} image.  The following example splits up the page
so that there is an \code{xyplot} beside a panel of text
(see Figure \ref{figure:trellisgrid}).  First of all, the
lattice plot is created, but
not drawn.  \grid{} is used to create some regions and the lattice
plot is drawn into one of those regions.

<<trellisgridplot, eval=FALSE>>=
someText <- paste("A panel of text", "produced using", "raw grid code",
                  "that could be used", "to describe",
                  "the plot", "to the right.", sep = "\n")
latticePlot <- xyplot(y ~ x | g, layout = c(2, 4))
grid.rect(gp = gpar(lty = "dashed"))
pushViewport(viewport(layout = grid.layout(1, 2,
                      widths = unit.c(unit(1, "strwidth", someText) +
                      unit(2, "cm"),
                      unit(1, "null")))))
pushViewport(viewport(layout.pos.col = 1))
grid.rect(gp = gpar(fill = "light grey"))
grid.text(someText,
          x = unit(1, "cm"), y = unit(1, "npc") - unit(1, "inches"),
          just = c("left", "top"))
popViewport()
pushViewport(viewport(layout.pos.col = 2))
print(latticePlot, newpage = FALSE)
popViewport(2)
<<trellisgrid, echo=FALSE, results=hide, fig=TRUE, width=6, height=6, include=FALSE>>=
<<trellisdata>>
<<trellisgridplot>>
@
\begin{figure}[tbp]
\begin{center}
{
\includegraphics[width=3.5in, height=3.5in]{grid-trellisgrid}
}
\end{center}
\caption{\label{figure:trellisgrid}
        A \lattice{} plot used as a component of a larger \grid{} image.}
\end{figure}

\end{document}
