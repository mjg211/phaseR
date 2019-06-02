#' Numerical solution and plotting
#'
#' Numerically solves a two-dimensional autonomous ODE system for a given
#' initial condition, using \code{\link[deSolve]{ode}} from the package
#' \code{\link[deSolve]{deSolve}}. It then plots the dependent variables against
#' the independent variable.
#'
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required structure of these
#' functions can be found in the package vignette, or in the help file for the
#' function \code{\link[deSolve]{ode}}.
#' @param y0 The initial condition. Should be a \code{\link[base]{numeric}}
#' \code{\link[base]{vector}} of \code{\link[base]{length}} two reflecting
#' the location of the two dependent variables initially.
#' @param tlim Sets the limits of the independent variable for which the
#' solution should be plotted. Should be a \code{\link[base]{numeric}}
#' \code{\link[base]{vector}} of \code{\link[base]{length}} two. If
#' \code{tlim[2] > tlim[1]}, then \code{tstep} should be negative to indicate a
#' backwards trajectory.
#' @param tstep The step length of the independent variable, used in numerical
#' integration. Decreasing the absolute magnitude of \code{tstep} theoretically
#' makes the numerical integration more accurate, but increases computation
#' time. Defaults to \code{0.01}.
#' @param parameters Parameters of the ODE system, to be passed to \code{deriv}.
#' Supplied as a \code{\link[base]{numeric}} \code{\link[base]{vector}}; the
#' order of the parameters can be found from the \code{deriv} file. Defaults to
#' \code{NULL}.
#' @param type If set to \code{"one"} the trajectories are plotted on the same
#' graph. If set to \code{"two"} they are plotted on separate graphs. Defaults
#' to \code{"one"}.
#' @param col Sets the colours of the trajectories of the two dependent
#' variables. Should be a \code{\link[base]{character}}
#' \code{\link[base]{vector}} of \code{\link[base]{length}} two. Will be reset
#' accordingly if it is of the wrong \code{\link[base]{length}}. Defaults to
#' \code{c("red", "blue")}.
#' @param add.grid Logical. If \code{TRUE}, grids are added to the plots.
#' Defaults to \code{TRUE}.
#' @param add.legend Logical. If \code{TRUE}, a \code{\link[graphics]{legend}}
#' is added to the plots. Defaults to \code{TRUE}.
#' @inheritParams .paramDummy
#' @param xlab Label for the x-axis of the resulting plot.
#' @param ylab Label for the y-axis of the resulting plot.
#' @param \dots Additional arguments to be passed to
#' \code{\link[graphics]{plot}}.
#' @return Returns a \code{\link[base]{list}} with the following components:
#' \item{add.grid}{As per input.}
#' \item{add.legend}{As per input.}
#' \item{col}{As per input, but with possible editing if a
#' \code{\link[base]{character}} \code{\link[base]{vector}} of the wrong
#' \code{\link[base]{length}} was supplied.}
#' \item{deriv}{As per input.}
#' \item{parameters}{As per input.}
#' \item{t}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} containing
#' the values of the independent variable at each integration step.}
#' \item{tlim}{As per input.}
#' \item{tstep}{As per input.}
#' \item{x}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} containing
#' the numerically computed values of the first dependent variable at each
#' integration step.}
#' \item{y}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} containing
#' the numerically computed values of the second dependent variable at each
#' integration step.}
#' \item{y0}{As per input.}
#' @author Michael J Grayling
#' @seealso \code{\link{ode}}, \code{\link{plot}}
#' @export
#' @examples
#' # A two-dimensional autonomous ODE system, vanDerPol.
#' vanDerPol_numericalSolution <- numericalSolution(vanDerPol,
#'                                                  y0         = c(4, 2),
#'                                                  tlim       = c(0, 100),
#'                                                  parameters = 3)
#'
numericalSolution <- function(deriv, y0 = NULL, tlim, tstep = 0.01,
                              parameters = NULL, type = "one",
                              col = c("red", "blue"), add.grid = TRUE,
                              add.legend = TRUE, state.names = c("x", "y"),
                              xlab = "t", ylab = state.names, ...) {
  if (any(tlim < 0)) {
    stop("tlim contains negative values")
  }
  if (tstep == 0) {
    stop("tstep is equal to 0")
  }
  if (tlim[1] == tlim[2]) {
    stop("tlim[1] is equal to tlim[2]")
  }
  if (all(tlim[1] > tlim[2], tstep > 0)) {
    stop("tstep must be negative if tlim[1] > tlim[2]")
  }
  if (all(tlim[1] < tlim[2], tstep < 0)) {
    stop("tstep must be positive if tlim[1] < tlim[2]")
  }
  if (is.null(y0)){
    y0 <- locator(n = 1)
    y0 <- c(y0$x, y0$y)
  }
  if ((!is.vector(y0)) & (!is.matrix(y0))){
    stop("y0 is not a vector or matrix as required")
  }
  if (is.vector(y0)){
    y0 <- as.matrix(y0)
  }
  if ((nrow(y0)*ncol(y0) != 2)){
    stop("y0 should be a matrix where nrow(y0)*ncol(y0) = 2 or a vector of ",
         "length two")
  }
  if (nrow(y0) < ncol(y0)){
    y0 <- t(y0)
  }
  if (!(type %in% c("one", "two"))){
    stop("type must either be set to \"one\" or \"two\"")
  }
  if (!is.vector(col)){
    stop("col is not a vector as required")
  }
  if (length(col) == 1){
    col            <- rep(col, 2)
    message("Note: col has been reset as required")
  }
  if (length(col) > 2) {
    col            <- col[1:2]
    message("Note: col has been reset as required")
  }
  if (!is.logical(add.grid)) {
    stop("add.grid must be set to either TRUE or FALSE")
  }
  if (!is.logical(add.legend)){
    stop("add.legend must be set to either TRUE or FALSE")
  }
  t                <- seq(tlim[1], tlim[2], tstep)
  phase.trajectory <- deSolve::ode(times  = t,
                                   y      = stats::setNames(y0, state.names),
                                   func   = deriv,
                                   parms  = parameters,
                                   method = "ode45")
  x                <- phase.trajectory[, 2]
  y                <- phase.trajectory[, 3]
  if (type == "one") {
    graphics::plot(t, x, col = col[1], type = "l", xlab = xlab,
                   ylab = paste0(ylab, collapse = ', '), ...)
    graphics::lines(t, y, col = col[2], type = "l", xlab = "t", ...)
    if (add.grid) {
      graphics::grid()
    }
    if (add.legend) {
      graphics::legend("topright", legend = state.names, lty = c(1, 1),
                       col = col)
    }
  }
  if (type == "two") {
    old.par        <- graphics::par(no.readonly = T)
    par(mfcol = c(2, 1))
    plot(t, x, col = col[1], type = "l", xlab = xlab, ylab = ylab[1], ...)
    if (add.grid) {
      graphics::grid()
    }
    graphics::plot(t, y, col = col[2], type = "l", xlab = xlab, ylab = ylab[2],
                   ...)
    if (add.grid) {
      graphics::grid()
    }
    par            <- old.par
  }
  return(list(add.grid   = add.grid,
              add.legend = add.legend,
              col        = col,
              deriv      = deriv,
              parameters = parameters,
              t          = t,
              tlim       = tlim,
              tstep      = tstep,
              type       = type,
              x          = x,
              y          = y,
              y0         = y0))
}
