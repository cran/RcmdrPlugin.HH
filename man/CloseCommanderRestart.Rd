\name{CloseCommanderRestart}
\alias{CloseCommanderRestart}
\alias{CloseCommanderNoQuestionRestart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Close Rcmdr without questions and then restart.}
\description{
  Close Rcmdr without questions.
  \code{CloseCommanderNoQuestionRestart} has absolutely no questions.
  \code{CloseCommanderRestart} asks only about saving files.
  Both functions restart Rcmdr immediately and therefore have the full
  \code{.GlobalEnv} from the R session still available.
}
\usage{
CloseCommanderNoQuestionRestart()
CloseCommanderRestart()
}
%- maybe also 'usage' for other objects documented here.
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[Rcmdr:Rcmdr.Utilities]{closeCommander}}}
\keyword{misc}
