\name{Xyplot.HH}
\alias{Xyplot.HH}
\alias{Xyplot.HH2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rcmdr Menu function to specify xyolot, other lattice plots, and likert plots.}
\description{
These are enhancements of the Rcmdr \code{Xyplot} function (which I
wrote) to include layout parameters and plot type, to force solid dots, and to
distinguish
between conditioning variables in the formula and group variables.
\code{Xyplot.HH} is an interface to the \code{xyplot} function.
\code{Xyplot.HH2} is an interface to many of the \code{lattice}
functions (\code{xyplot}, \code{bwplot}, \code{splom},
\code{barchart}, \code{dotplot}) and to the formula method for
\code{likert} in the \code{HH} package.
When either \code{barchart} or \code{panel.barchart} is selected, then the argument \code{origin=0} is
automatically set.  When \code{panel.barchart}, the user must manually
specify the limits (\code{xlim} or \code{ylim}) to include zero for the effect of \code{origin=0} to
be visible.
}
\usage{
Xyplot.HH()
Xyplot.HH2()
}
%- maybe also 'usage' for other objects documented here.
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[lattice]{xyplot}}, \code{\link[HH]{likert}}}
\keyword{hplot}
