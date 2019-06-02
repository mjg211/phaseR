#' Stable and unstable manifolds
#'
#' Plots the stable and unstable manifolds of a saddle point. A search
#' procedure is utilised to identify an equilibrium point, and if it is a saddle
#' then its manifolds are added to the plot.
#'
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required structure of these
#' functions can be found in the package vignette, or in the help file for the
#' function \code{\link[deSolve]{ode}}.
#' @param y0 The initial point from which a saddle will be searched for. This
#' can either be a \code{\link[base]{numeric}} \code{\link[base]{vector}} of
#' \code{\link[base]{length}} two, reflecting the location of the two
#' dependent variables, or alternatively this can be specified as
#' \code{\link[base]{NULL}}, and then \code{\link[graphics]{locator}} can be
#' used to specify the initial point on a plot. Defaults to
#' \code{\link[base]{NULL}}.
#' @param parameters Parameters of the ODE system, to be passed to \code{deriv}.
#' Supplied as a \code{\link[base]{numeric}} \code{\link[base]{vector}}; the
#' order of the parameters can be found from the \code{deriv} file. Defaults to
#' \code{\link[base]{NULL}}.
#' @param tstep The step length of the independent variable, used in numerical
#' integration. Decreasing the absolute magnitude of \code{tstep} theoretically
#' makes the numerical integration more accurate, but increases computation
#' time. Defaults to \code{0.01}.
#' @param tend The final time of the numerical integration performed to
#' identify the manifolds.
#' @param col Sets the colours used for the stable and unstable manifolds.
#' Should be a \code{\link[base]{character}} \code{\link[base]{vector}} of
#' \code{\link[base]{length}} two. Will be reset accordingly if it is of the
#' wrong \code{\link[base]{length}}. Defaults to \code{c("green", "red")}.
#' @param add.legend Logical. If \code{TRUE}, a legend is added to the plots.
#' Defaults to \code{TRUE}.
#' @inheritParams .paramDummy
#' @param ... Additional arguments to be passed to plot.
#' @return Returns a \code{\link[base]{list}} with the following components:
#' \item{add.legend}{As per input.}
#' \item{col}{As per input, but with possible editing if a
#' \code{\link[base]{character}} \code{\link[base]{vector}} of the wrong
#' \code{\link[base]{length}} was supplied.}
#' \item{deriv}{As per input.}
#' \item{parameters}{As per input.}
#' \item{stable.1}{A \code{\link[base]{numeric}} \code{\link[base]{matrix}}
#' whose columns are the numerically computed values of the dependent variables
#' for part of the stable manifold.}
#' \item{stable.2}{A \code{\link[base]{numeric}} \code{\link[base]{matrix}}
#' whose columns are the numerically computed values of the dependent variables
#' for part of the stable manifold.}
#' \item{tend}{As per input.}
#' \item{unstable.1}{A \code{\link[base]{numeric}} \code{\link[base]{matrix}}
#' whose columns are the numerically computed values of the dependent variables
#' for part of the unstable manifold.}
#' \item{unstable.2}{A \code{\link[base]{numeric}} \code{\link[base]{matrix}}
#' whose columns are the numerically computed values of the dependent variables
#' for part of the unstable manifold.}
#' \item{y0}{As per input.}
#' \item{ystar}{Location of the identified equilibrium point.}
#' @author Michael J Grayling, Stephen P Ellner, John M Guckenheimer
#' @export
drawManifolds <- function(deriv, y0 = NULL, parameters = NULL, tstep = 0.1,
                          tend = 100, col = c("green", "red"),
                          add.legend = TRUE, state.names = c("x", "y"), ...) {
  if (is.null(y0)) {
    y0         <- locator(n = 1)
    y0         <- c(y0$x, y0$y)
  }
  if (all(!is.vector(y0), !is.matrix(y0))) {
    stop("y0 is not a vector or matrix, as is required")
  }
  if (is.vector(y0)) {
    y0         <- as.matrix(y0)
  }
  if (nrow(y0)*ncol(y0) != 2) {
    stop("y0 should be a matrix where nrow(y0)*ncol(y0) = 2 or a vector of ",
         "length two")
  }
  if (nrow(y0) < ncol(y0)) {
    y0         <- t(y0)
  }
  if (length(col) != 2) {
    if (length(col) == 1) {
      col      <- rep(col, 2)
    }
    if (length(col) > 2) {
      col      <- col[1:2]
    }
    message("Note: col has been reset as required")
  }
  if (tend <= 0) {
    stop("tend is less than or equal to zero")
  }
  if (!is.logical(add.legend)) {
    stop("add.legend must be logical")
  }
  find.ystar   <- findEquilibrium(deriv = deriv, y0 = y0, system = "two.dim",
                                  parameters = parameters, plot.it = F,
                                  summary = F, state.names = state.names)
  ystar        <- find.ystar$ystar
  jacobian     <- find.ystar$jacobian
  eigenvectors <- find.ystar$eigenvectors
  eigenvalues  <- find.ystar$eigenvalues
  if (eigenvalues[1]*eigenvalues[2] > 0) {
    message("Fixed point is not a saddle")
  } else {
    i1         <- which(eigenvalues > 0)
    i2         <- which(eigenvalues < 0)
    v1         <- eigenvectors[, i1]
    v2         <- eigenvectors[, i2]
    eps        <- 1e-2
    ymax       <- 0.5 + max(abs(ystar))
    maxtime.1  <- max(tend, log(2500*ymax)/abs(eigenvalues[i1]))
    out.1      <- deSolve::ode(times = seq(0, maxtime.1, tstep),
                               y     = setNames(ystar + eps*v1, state.names),
                               func  = deriv,
                               parms = parameters)
    graphics::lines(out.1[, 2], out.1[, 3], type = "l", col = col[2], ...)
    unstable.1 <- out.1[, 1:3]
    out.2      <- deSolve::ode(times = seq(0, maxtime.1, tstep),
                               y     = setNames(ystar - eps*v1, state.names),
                               func  = deriv,
                               parms = parameters)
    graphics::lines(out.2[, 2], out.2[, 3], type = "l", col = col[2], ...)
    unstable.2 <- out.2[, 1:3]
    maxtime.2  <- max(tend, log(2500*ymax)/abs(eigenvalues[i2]))
    out.3      <- deSolve::ode(times = -seq(0, maxtime.2, tstep),
                               y     = setNames(ystar + eps*v2, state.names),
                               func  = deriv,
                               parms = parameters)
    graphics::lines(out.3[, 2], out.3[, 3], type = "l", col = col[1], ...)
    stable.1   <- out.3[, 1:3]
    out.4      <- deSolve::ode(times = -seq(0, maxtime.2, tstep),
                               y     = setNames(ystar - eps*v2, state.names),
                               func  = deriv,
                               parms = parameters)
    graphics::lines(out.4[, 2], out.4[, 3], type = "l", col = col[1], ...)
    stable.2   <- out.4[, 1:3]
    if (add.legend) {
      graphics::legend("topright", c("Stable", "Unstable"), lty = 1, lwd = 1,
                       col = col)
    }
    return(list(add.legend = add.legend,
                col        = col,
                deriv      = deriv,
                parameters = parameters,
                stable.1   = stable.1,
                stable.2   = stable.2,
                tend       = tend,
                unstable.1 = unstable.1,
                unstable.2 = unstable.2,
                y0         = y0,
                ystar      = ystar))
  }
}
