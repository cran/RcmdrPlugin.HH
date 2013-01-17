\name{twoWayTable.HH}
\alias{twoWayTable.HH}
\alias{analyzeTwoWayTable.HH}
\alias{enterTable.HH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rcmdr menu interface to chisq.test}
\description{
Pearson's Chi-squared Test for Count Data

\code{twoWayTable.HH} is an original Rcmdr.HH function.  It reads
the active dataset and constructs the table using \code{xtabs}.

\code{enterTable.HH} is an original Rcmdr.HH function.  It opens a window
where the user may enter a table manually.

\code{analyzeTwoWayTable.HH} is an additional function.  It uses the
active dataset as the table.

All three produce identical output, a two-way table, row and column
summaries,
and the chi square test.
}
\usage{
twoWayTable.HH()

enterTable.HH()

analyzeTwoWayTable.HH()
}
%- maybe also 'usage' for other objects documented here.
\author{John Fox \email{jfox@mcmaster.ca}.
  additional entry options by Richard M. Heiberger <rmh@temple.edu>.}
\seealso{ \code{\link[stats]{chisq.test}}}
\keyword{ htest }
