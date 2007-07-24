\name{scatter3d.HH}
\alias{scatter3d.HH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Three-Dimensional Scatterplots and Point Identification}
\description{
  The \code{scatter3d} function uses the \code{rgl} package to draw 3D scatterplots
  with various regression surfaces. The function \code{identify3d} 
  allows you to label points interactively with the mouse:
  Press the right mouse button (on a two-button mouse) or the centre button (on a
  three-button mouse), drag a 
  rectangle around the points to be identified, and release the button. 
  Repeat this procedure for each point or
  set of ``nearby'' points to be identified. To exit from point-identification mode,
  click the right (or centre) button an empty region of the plot.

  This is a revision of the \code{Rcmdr} \code{scatter3d} to add the 
ability to plot squared residuals.}
\usage{
scatter3d.HH(x, y, z, 
    xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),
    zlab=deparse(substitute(z)),
    revolutions=0, bg.col=c("white", "black"), 
    axis.col=if (bg.col == "white") "black" else "white",
    surface.col=c("blue", "green", "orange", "magenta", 
        "cyan", "red", "yellow", "gray"),
    neg.res.col="red", pos.res.col="green", point.col="yellow",
    text.col=axis.col, 
    grid.col=if (bg.col == "white") "black" else "gray",
    fogtype=c("exp2", "linear", "exp", "none"),
    residuals=(length(fit) == 1), surface=TRUE, grid=TRUE, 
    grid.lines=26, df.smooth=NULL, df.additive=NULL,
    sphere.size=1, threshold=0.01, speed=1, fov=60,
    fit="linear", groups=NULL, parallel=TRUE, model.summary=FALSE,
    squares = FALSE, square.color = "gray", coef.ratio = 1)
}
\note{
You have to install the \code{rgl} and \code{mgcv} packages to produce 3D plots. 
}
\arguments{
  \item{x}{variable for horizontal axis.}
  \item{y}{variable for vertical axis (response).}
  \item{z}{variable for out-of-screen axis.}
  \item{xlab, ylab, zlab}{axis labels.}
  \item{revolutions}{number of full revolutions of the display.}
  \item{bg.col}{background colour; one of \code{"white"}, \code{"black"}.}
  \item{axis.col}{colour for axes; default is \code{"white"} for black background, 
    \code{"black"} for white background.}
  \item{surface.col}{vector of colours for regression planes, 
    used in the order specified by \code{fit}.}
  \item{neg.res.col, pos.res.col}{colours for lines representing negative 
    and positive residuals.}
  \item{point.col}{colour of points.}
  \item{text.col}{colour of axis labels.}
  \item{grid.col}{colour of grid lines on the regression surface(s).}
  \item{fogtype}{type of fog effect; one of \code{"exp2"}, \code{"linear"}, 
    \code{"exp"}, \code{"none".}}
  \item{residuals}{plot residuals (\code{TRUE} or \code{FALSE}); available only 
    when there is one surface plotted.}
  \item{surface}{plot surface(s) (\code{TRUE} or \code{FALSE}).}
  \item{grid}{plot grid lines on the regression surface(s) (\code{TRUE} or \code{FALSE}).}
  \item{grid.lines}{number of lines (default, 26) forming the grid, in each of the x and y directions.}
  \item{df.smooth}{degrees of freedom for the two-dimensional smooth regression surface;
    if \code{NULL} (the default), the \code{\link[mgcv]{gam}} function will select the degrees of freedom
    for a smoothing spline by generalized cross-validation; if a positive number, a fixed
    regression spline will be fit with the specified degrees of freedom.}
  \item{df.additive}{degrees of freedom for each explanatory variable in an additive regression;
    if \code{NULL} (the default), the \code{gam} function will select degrees of freedom 
    for the smoothing splines by generalized cross-validation; if a positive number
    or a vector of two positive numbers, fixed
    regression splines will be fit with the specified degrees of freedom for each term.}
  \item{sphere.size}{relative sizes of spheres representing points; 
    the actual size is dependent on the number of observations.}
  \item{threshold}{if the actual size of the spheres is less than the threshold, 
    points are plotted instead.}
  \item{speed}{relative speed of revolution of the plot.}
  \item{fov}{field of view (in degrees); controls degree of perspective.}
  \item{fit}{one or more of \code{"linear"}, \code{"quadratic"}, \code{"smooth"}, 
    \code{"additive"}; to display fitted surface(s); partial matching is supported -- 
    e.g., \code{c("lin", "quad")}.}
  \item{groups}{if \code{NULL} (the default), no groups are defined; if a factor, a different surface
    or set of surfaces is plotted for each level of the factor; in this event, the colours in 
    \code{plane.col} are used successively for the points, surfaces, and residuals corresponding to
    each level of the factor.}
  \item{parallel}{when plotting surfaces by \code{groups}, should the surfaces be constrained to be
    parallel? A logical value, with default \code{TRUE}.}
  \item{model.summary}{print summary or summaries of the model(s) fit 
    (\code{TRUE} or \code{FALSE}).}
  \item{col}{colours for the point labels, given by group. There must be at
    least as many colours as groups; if there are no groups, the first colour is used. Normally, the colours
    would correspond to the \code{plane.col} argument to \code{scatter3d}.}
  \item{squares}{logical.  If \code{TRUE}, the residuals are plotted as
    squares.
    The sum of the area of the squares is the "residual sum of squares".
  If \code{FALSE}, the residuals are plotted as vertical lines.}
  \item{square.color}{color for the squares.}

  \item{coef.ratio}{number, defaults to 1.  Settig \code{coef.ratio} to a number
    other than 1 is a primitive way of plotting squared
    pseudo-residuals that are not the least-squares residuals.  The
    reason for displaying non-least-squares residuals is to compare them
    to the least-squares residuals and thus get a 3d visual image of
    what minimizing the sum of squares actually means.}
}

\value{
  \code{scatter3d} not return a useful value; it is used for its side-effect of
  creating a 3D scatterplot. \code{indentify3d} returns the labels of the
  identified points.
}

\author{John Fox \email{jfox@mcmaster.ca}.  Squared residuals added by
  Richard M. Heiberger <rmh@temple.edu>.}

\seealso{\code{\link[rgl]{rgl.open}}, \code{\link[mgcv]{gam}}}

\examples{
    \dontrun{
State.x77 <- as.data.frame(state.x77)
with(State.x77, scatter3d(Income, Murder, Illiteracy))
with(State.x77, identify3d(Income, Murder, Illiteracy, labels=row.names(State.x77)))
with(State.x77,  scatter3d(Income, Murder, Illiteracy, fit=c("linear", "quadratic")))
    }
}

\keyword{hplot}
