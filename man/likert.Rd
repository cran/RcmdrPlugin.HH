\name{PlotLikertDialog}
\alias{PlotLikertDialog}
\alias{listAllLikertCapable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rcmdr Menu function to specify a likert plot.}
\description{
  Please see \code{\link[HH]{likert}} for details on the
  \code{plot.likert} and related functions.
}
\usage{
PlotLikertDialog()
listAllLikertCapable(envir = .GlobalEnv, ...)
}
%- maybe also 'usage' for other objects documented here.
\value{
  For \code{listAllLikertCapable}, a character vector of names of all
  objects that satisfy the search criteria in the specified
  environments.
  See \code{\link[HH]{likert}} for details on what objects are likert capable.
}
\arguments{
  \item{envir, \dots}{Arguments to \code{\link[base]{ls}}.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[HH]{likert}}}
\keyword{hplot}
