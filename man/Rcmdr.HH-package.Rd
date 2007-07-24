\name{Rcmdr.HH-package}
\alias{Rcmdr.HH-package}
\alias{Rcmdr.HH}
\docType{package}
\title{
Functions added to the Rcmdr package to support the introductory course
at Temple University.
}
\description{
  Our introductory course spends time on several topics that are not yet
  in the R Commander.  Therefore we wrote the menu items and make them
  available.
}
\details{
\tabular{ll}{
Package: \tab Rcmdr.HH\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2006-06-02\cr
License: \tab GPL version 2 or newer?\cr
}
% older scheme, not currently used
%
% Backup the \code{Rcmdr/etc/Rcmdr-menus.txt} file to, for example
% \code{Rcmdr/etc/Rcmdr-menus-1.7.txt}.  Then copy the file
% \code{Rcmdr.HH/etc/Rcmdr-menus.txt} to \code{Rcmdr/etc/}.
% Load this package earlier in the \code{search} sequence than
% \code{library(Rcmdr)}.

\code{\link{bestSubsetsRegressionModel.HH}} Rcmdr interface to the
\code{\link[leaps]{regsubsets}} function in the \code{leaps} package.

\code{\link{twoWayTable.HH}} Pearson's Chi-squared Test for Count Data
(additional formats for data input)

\code{\link{anovaTableI.HH}} Sequential sums of squares on the Rcmdr menu.

\code{\link{scatter3d.HH}} add the ability to plot squared residuals.
The squared residuals have been adopted into \code{Rcmdr}.  This
interface offers a checkbox for a new 3D window and an option to
draw a non-least-squares plane for pedagogical comparison.

\code{\link[HH]{ci.plot}} Plot confidence and prediction intervals for
simple linear regression.

\code{\link[HH]{panel.bwplot.hh}} Panel functions for \code{bwplot} that
  give the user control over the placement of the boxes.

\code{\link[HH]{interaction2wt}} Plot all main effects and twoway
interactions in a multifactor design.

\code{\link{scatterPlotMatrix.HH}} Similar to
\code{\link[Rcmdr]{scatterPlotMatrix}}
  The revision uses \code{row1attop=FALSE} to force the main diagonal of
  the scatterplot matrix to go uphill from southwest to northeast.

\code{\link{QQPlot.HH}}  Added Shapiro-Wilk test of normality.

\code{\link[HH]{norm.curve}} Plot a normal curve with shaded rejection regions,
optionally a second curve centered at an alternative hypothesis value
can be plotted.  Both \code{x} and \code{z} scales are displayed.

}

\author{
Richard M. Heiberger, with contributions from Burt Holland

Maintainer: Richard M. Heiberger <rmh@temple.edu>
}
\references{
Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\keyword{ package }
\seealso{
\code{\link[Rcmdr]{Rcmdr}}
}

\examples{
\dontrun{
   ## start R
   library(Rcmdr.HH)        ## loads the package and opens the Rcmdr
                            ## window with the HH menu
}
}
