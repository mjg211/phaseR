#' Numerical Solution and Plotting
#' 
#' Numerically solves a two dimensional autonomous ODE system for a given
#' initial condition, using ode from the package deSolve. It then plots the
#' dependent variables against the independent variable.
#' 
#' 
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required structure of these
#' functions can be found in the package guide.
#' @param y0 The initial condition. Should be a vector of length two reflecting
#' the location of the two dependent variables initially.
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
#' @param type If set to "one" the trajectories are plotted on the same graph.
#' If set to "two" they are plotted on separate graphs. Defaults to "one".
#' @param col Sets the colours of the trajectories of the two dependent
#' variables. Will be reset accordingly if it is not a vector of length two.
#' Defaults to c("red", "blue").
#' @param add.grid Logical. If TRUE, grids are added to the plots. Defaults to
#' TRUE.
#' @param add.legend Logical. If TRUE, a legend is added to the plots. Defaults
#' to TRUE.
#' @param \dots Additional arguments to be passed to plot.
#' @inheritParams .paramDummy
#' @return Returns a list with the following components: \item{add.grid}{As per
#' input.} \item{add.legend}{As per input.} \item{col}{As per input, but with
#' possible editing if a colour vector of the wrong length was supplied.}
#' \item{deriv}{As per input.} \item{parameters}{As per input.} \item{t}{A
#' vector containing the values of the independent variable at each integration
#' step.} \item{tlim}{As per input.} \item{tstep}{As per input.} \item{x}{A
#' vector containing the numerically computed values of the first dependent
#' variable at each integration step.} \item{y}{A vector containing the
#' numerically computed values of the second dependent variable at each
#' integration step.} \item{y0}{As per input.}
#' @author Michael J. Grayling
#' @seealso \code{\link{ode}}, \code{\link{plot}}
#' @export
#' @examples
#' # A two dimensional autonomous ODE system, vanDerPol.
#' vanDerPol.numericalSolution <- numericalSolution(vanDerPol, y0 = c(4, 2), tlim = c(0, 100),
#'                                                  parameters = 3)
#' 
numericalSolution <- function(deriv, y0 = NULL, tlim, tstep = 0.01,
                              parameters = NULL, type = "one",
                              col = c("red", "blue"), add.grid = TRUE, 
                              add.legend = TRUE, state.names = c("x", "y"), ...){
  if (any(tlim < 0)){
    stop("tlim contains negative values")
  }
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
    stop("y0 should be a matrix where nrow(y0)*ncol(y0) = 2 or a vector of length two")
  }
  if (nrow(y0) < ncol(y0)){
    y0 <- transpose(y0)
  }
  if (!(type %in% c("one", "two"))){
    stop("type must either be set to \"one\" or \"two\"")
  }
  if (!is.vector(col)){
    stop("col is not a vector as required")
  }
  if (length(col) == 1){
    col <- rep(col, 2)
    print("Note: col has been reset as required")
  }
  if (length(col) > 2){
    col <- col[1:2]
    print("Note: col has been reset as required")
  }
  if (!is.logical(add.grid)){
    stop(paste("add.grid must either be set to TRUE or FALSE"))
  }
  if (!is.logical(add.legend)){
    stop(paste("add.legend must either be set to TRUE or FALSE"))
  }
  t                <- seq(from = tlim[1], to = tlim[2], by = tstep)
  phase.trajectory <- ode(times = t, y = setNames(y0, state.names), 
                          func = deriv, parms = parameters, method = "ode45")
  x                <- phase.trajectory[, 2]
  y                <- phase.trajectory[, 3]
  if (type == "one"){
    plot(t, x, col = col[1], type = "l", xlab = "t", ylab = "x, y", ...)
    lines(t, y, col = col[2], type = "l", xlab = "t", ylab = "x, y", ...)
    if (add.grid == TRUE){
      grid()
    }
    if (add.legend == TRUE){
      legend("topright", legend = c("x", "y"), lty = c(1, 1),
             col = col)
    }
  }
  if (type == "two"){
    old.par <- par(no.readonly = TRUE)
    par(mfcol = c(2, 1))
    plot(t, x, col = col[1], type = "l", xlab = "t", ylab = "x", ...)
    if (add.grid == TRUE){
      grid()
    }
    plot(t, y, col = col[2], type = "l", xlab = "t", ylab = "y", ...)
    if (add.grid == TRUE){
      grid()
    }
    par <- old.par
  }
  return(list(add.grid = add.grid, add.legend = add.legend, col = col,
              deriv = deriv, parameters = parameters, t = t, tlim = tlim,
              tstep = tstep, type = type, x = x, y = y, y0 = y0))
}
