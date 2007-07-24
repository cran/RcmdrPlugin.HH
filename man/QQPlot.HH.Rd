\name{QQPlot.HH}
\alias{QQPlot.HH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Quantile-Comparison (QQ) Plot}
\description{
Rcmdr menu interface to plot the qqplot of variable against one of the following distributions: normal, t, chi-square, F, other.
}
\usage{
QQPlot.HH()
}
%- maybe also 'usage' for other objects documented here.
\details{
  The \code{normal} gives the option to do the Shapiro-Wilk test of normality.
  The \code{other} requires you to specify the distribution.
  Any distribution for which quantile and density functions exist in
  R (with prefixes \code{q} and \code{d}, respectively) may be used. 
}
\value{
\code{NULL}. These functions are used only for their side effect (to
     make a graph).
}
\author{John Fox \email{jfox@mcmaster.ca}.
  Shapiro--Wilk test added by Richard M. Heiberger <rmh@temple.edu>.}
\seealso{\code{\link[car]{qq.plot}}, \code{\link[stats]{shapiro.test}}}
\keyword{ hplot }
\keyword{ distribution }
