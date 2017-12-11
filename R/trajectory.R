#' Phase Plane Trajectory Plotting
#' 
#' Performs numerical integration of the chosen ODE system, for a user
#' specified set of initial conditions. Plots the resulting solution(s) in the
#' phase plane.
#' 
#' 
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required structure of these
#' functions can be found in the package guide.
#' @param y0 The initial condition(s). In the case of a one dimensional system,
#' this can either be a single number indicating the location of the dependent
#' variable initially, or a vector indicating multiple initial locations of the
#' independent variable. In the case of a two dimensional system, this can
#' either be a vector of length two reflecting the location of the two
#' dependent variables initially. Or it can be matrix where each row reflects a
#' different initial condition. Alternatively this can be left blank and the
#' user can use locator to specify initial condition(s) on a plot. In this
#' case, for one dimensional systems, all initial conditions are taken at
#' tlim[1], even if not selected so on the graph. Defaults to NULL.
#' @param n If y0 is left NULL so initial conditions can be specified using
#' locator, n sets the number of initial conditions to be chosen. Defaults to
#' NULL.
#' @param tlim Sets the limits of the independent variable for which the
#' solution should be plotted. Should be a vector of length two. If tlim[2] >
#' tlim[1], then tstep should be negative to indicate a backwards trajectory.
#' @param tstep The step length of the independent variable, used in numerical
#' integration. Decreasing the absolute magnitude of tstep theoretically makes
#' the numerical integration more accurate, but increases computation time.
#' Defaults to 0.01.
#' @param parameters Parameters of the ODE system, to be passed to deriv.
#' Supplied as a vector; the order of the parameters can be found from the
#' deriv file. Defaults to NULL.
#' @param system Set to either "one.dim" or "two.dim" to indicate the type of
#' system being analysed. Defaults to "two.dim".
#' @param col The colour(s) to plot the trajectories in. Will be reset
#' accordingly if it is a vector not of the length of the number of initial
#' conditions. Defaults to "black".
#' @param add Logical. If TRUE, the trajectories added to an existing plot. If
#' FALSE, a new plot is created. Defaults to TRUE.
#' @param \dots Additional arguments to be passed to plot.
#' @inheritParams .paramDummy
#' 
#' @return Returns a list with the following components (the exact make up is
#' dependent upon the type of system being analysed): \item{add}{As per input.}
#' \item{col}{As per input, but with possible editing if a colour vector of the
#' wrong length was supplied.} \item{deriv}{As per input.} \item{n}{As per
#' input.} \item{parameters}{As per input.} \item{system}{As per input.}
#' \item{tlim}{As per input.} \item{tstep}{As per input.} \item{t}{A vector
#' containing the values of the independent variable at each integration step.}
#' \item{x}{In the two dimensional system casem a matrix whose columns are the
#' numerically computed values of the first dependent variable for each initial
#' condition.} \item{y}{In the two dimensional system case, a matrix whose
#' columns are the numerically computed values of the second dependent variable
#' for each initial condition. In the one dimensional system case, a matrix
#' whose columns are the numerically computed values of the dependent variable
#' for each initial condition.} \item{y0}{As per input, but converted to a
#' matrix if supplied as a vector initially.}
#' @author Michael J. Grayling
#' @seealso \code{\link{ode}}, \code{\link{plot}}
#' @export
#' @examples
#' # Plot the flow field, nullclines and several trajectories for the one
#' # dimensional autonomous ODE system logistic.
#' logistic.flowField <- flowField(logistic, x.lim = c(0, 5), y.lim = c(-1, 3),
#'                                 parameters = c(1, 2), points = 21, system = "one.dim",
#' 								add = FALSE)
#' logistic.nullclines <- nullclines(logistic, x.lim = c(0, 5), y.lim = c(-1, 3),
#'                                   parameters = c(1, 2), system = "one.dim")
#' logistic.trajectory <- trajectory(logistic, y0 = c(-0.5, 0.5, 1.5, 2.5), t.end = 5,
#'                                   parameters = c(1, 2), system = "one.dim")
#' 
#' # Plot the velocity field, nullclines and several trajectories for the two dimensional
#' # autonomous ODE system simplePendulum.
#' simplePendulum.flowField  <- flowField(simplePendulum, x.lim = c(-7, 7),
#'                                        y.lim = c(-7, 7), parameters = 5, points = 19,
#' 									   add = FALSE)
#' y0                        <- matrix(c(0, 1, 0, 4, -6, 1, 5, 0.5, 0, -3), ncol = 2,
#'                                     nrow = 5, byrow = TRUE)
#' simplePendulum.nullclines <- nullclines(simplePendulum, x.lim = c(-7, 7),
#'                                         y.lim = c(-7, 7), parameters = 5, points = 500)
#' simplePendulum.trajectory <- trajectory(simplePendulum, y0 = y0, t.end = 10,
#'                                         parameters = 5)
#' 
trajectory <- function(deriv, y0 = NULL, n = NULL, tlim, tstep = 0.01, 
                       parameters = NULL, system = "two.dim",
                       col = "black", add = TRUE, state.names = c("x", "y"), 
                       ...){
  if (tstep == 0){
    stop("tstep is equal to 0")
  }
  if (tlim[1] == tlim[2]){
    stop("tlim[1] is equal to tlim[2]")
  }
  if ((tlim[1] > tlim[2]) & (tstep > 0)){
    stop("tstep must be negative if tlim[1] > tlim[2]")
  }
  if ((tlim[1] < tlim[2]) & (tstep < 0)){
    stop("tstep must be positive if tlim[1] < tlim[2]")
  }
  if (!(system %in% c("one.dim", "two.dim"))){
    stop("system must either be set to one.dim or two.dim")
  }
  if (!is.vector(col)){
    stop("col is not a vector as required")
  }
  if (!is.logical(add)){
    stop("add must be logical")
  }
  if (is.null(y0) & is.null(n)){
    stop(paste("Both y0 and n cannot be NULL"))
  }
  if (!is.null(y0) & !is.null(n)){
    warning("n is non-NULL whilst y0 has also been specified")
  }
  if (is.null(y0) & (add == FALSE)){
    stop(paste("y0 cannot be null and add set to FALSE"))
  }
  if (is.null(y0)){
    y0 <- locator(n = n)
    if (system == "two.dim"){
      re.set <- matrix(0, ncol = 2, nrow = n)
      for (i in 1:n){
        re.set[i, ] <- c(y0$x[i], y0$y[i])
      }
      y0 <- re.set
    }
    if (system == "one.dim"){
      re.set <- numeric(n)
      for (i in 1:n){
        re.set[i] <- y0$y[i]
      }
      y0 <- re.set
    }
  }
  if ((!is.vector(y0)) & (!is.matrix(y0))){
    stop("y0 is neither a number, vector or matrix as required")
  }
  if (is.vector(y0)){
    y0   <- as.matrix(y0)
  }
  if ((system == "one.dim") & (all(dim(y0) > 1))){
    stop("For system equal to \"one.dim\" y0 must contain either a vector or a matrix where either nrow(y0) or ncol(y0) is one")
  }
  if ((system == "two.dim") & (!any(dim(y0) == 2))){
    stop("For system equal to \"two.dim\" y0 must contain either a vector of length two or a matrix where either nrow(y0) or ncol(y0) is two")
  }
  if (system == "one.dim"){
    if (ncol(y0) > nrow(y0)){
      y0 <- t(y0)
    }
    state.names <- state.names[1]
  } else {
    if ((nrow(y0) == 2) & (ncol(y0) != 2)){
      y0 <- t(y0)
    }
  }
  if (nrow(y0) > length(col)){
    col <- rep(col, nrow(y0))
    message("Note: col has been reset as required")
  } else if (nrow(y0) < length(col)) {
    col <- col[1:nrow(y0)]
    message("Note: col has been reset as required")
  }
  t <- seq(from = tlim[1], to = tlim[2], by = tstep)
  x <- matrix(0, nrow = length(t), ncol = nrow(y0))
  if (system == "two.dim"){
    y  <- matrix(0, nrow = length(t), ncol = nrow(y0))
  }
  method <- ifelse(tstep > 0, "ode45", "lsoda")
  for (i in 1:nrow(y0)){
    phase.trajectory <- ode(times = t, y = setNames(c(y0[i, ]), state.names), 
                            func = deriv, parms = parameters, method = method)
    x[, i]   <- phase.trajectory[, 2]
    if (system == "two.dim"){
      y[, i] <- phase.trajectory[, 3]
    }
    if ((add == FALSE) & (i == 1)){
      if (system == "one.dim"){
        plot(t, x[, i], col = col[i], type = "l", ...)
      } else {
        plot(x[, i], y[, i], col = col[i], type = "l", ...)
      }
    } else {
      if (system == "one.dim"){
        lines(t, x[, i], col = col[i], type = "l", ...)
      } else {
        lines(x[, i], y[, i], col = col[i], type = "l", ...)
      }
    }
  }
  if (system == "one.dim"){
    points(rep(tlim[1], nrow(y0)), y0, col = col, ...)
    return(list(add = add, col = col, deriv = deriv, n = n,
                parameters = parameters, system = system, t = t, tlim = tlim,
                tstep = tstep, y = x, y0 = y0))
  } else {
    points(y0[, 1], y0[, 2], col = col, ...)
    return(list(add = add, col = col, deriv = deriv, n = n,
                parameters = parameters, system = system, t = t, tlim = tlim,
                tstep = tstep, x = x, y = y, y0 = y0))
  }
}
