\name{ResizeEtcDialog}
\alias{ResizeEtcDialog}
\alias{listAllTrellisObjects}
\title{Rcmdr Menu function to specify combining and resizing "trellis" objects.}
\description{
  Please see \code{\link[HH]{ResizeEtc}} for details on the combination
  of \code{"trellis"} and related functions.
}
\usage{
ResizeEtcDialog()
listAllTrellisObjects(envir = .GlobalEnv, ...)
}
\arguments{
  \item{envir, \dots}{Arguments to \code{\link[base]{ls}}.}
}
\details{
  This dialog is a template designed to help with writing commandline code.
}
\value{
  For \code{listAllTrellisObjects}, a character vector of names of all
  \code{"trellis"} objects that satisfy the search criteria in the specified environments.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[HH]{ResizeEtc}}, \code{\link[latticeExtra]{resizePanels}}}
\keyword{hplot}
