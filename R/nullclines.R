#' Nullclines
#' 
#' Plots nullclines for two dimensional autonomous ODE systems. Or can be used
#' to plot horizontal lines at equilibrium points for one dimensional
#' autonomous ODE systems.
#' 
#' 
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required structure of these
#' functions can be found in the package guide.
#' @param xlim In the case of a two dimensional system, this sets the limits of
#' the first dependent variable in which gradient reflecting line segments
#' should be plotted. In the case of a one dimensional system, this sets the
#' limits of the independent variable in which these line segments should be
#' plotted. Should be a vector of length two.
#' @param ylim In the case of a two dimensional system this sets the limits of
#' the second dependent variable in which gradient reflecting line segments
#' should be plotted. In the case of a dimensional system, this sets the limits
#' of the dependent variable in which these line segments should be plotted.
#' Should be a vector of length two.
#' @param parameters Parameters of the ODE system, to be passed to deriv.
#' Supplied as a vector; the order of the parameters can be found from the
#' deriv file. Defaults to NULL.
#' @param points Sets the density at which derivatives are computed. points x
#' points derivatives will be computed. Levels of zero gradient are identified
#' using these computations and the function contour. Increasing the value of
#' points improves identification of nullclines, but increases computation
#' time. Defaults to 101.
#' @param system Set to either "one.dim" or "two.dim" to indicate the type of
#' system being analysed. Defaults to "two.dim".
#' @param col In the case of a two dimensional system, sets the colours used
#' for the x- and y-nullclines. In the case of a one dimensional system, sets
#' the colour of the lines plotted horizontally along the equilibria. Will be
#' reset accordingly if it is a vector of the wrong length. Defaults to
#' c("blue", "cyan").
#' @param add Logical. If TRUE, the nullclines are added to an existing plot.
#' If FALSE, a new plot is created. Defaults to TRUE.
#' @param add.legend Logical. If TRUE, a legend is added to the plots. Defaults
#' to TRUE.
#' @param \dots Additional arguments to be passed to either plot or contour.
#' @inheritParams .paramDummy
#' @return Returns a list with the following components (the exact make up is
#' dependent upon the value of system): \item{add}{As per input.}
#' \item{add.legend}{As per input.} \item{col}{As per input, but with possible
#' editing if a vector of the wrong length was supplied.} \item{deriv}{As per
#' input.} \item{dx}{A matrix. In the case of a two dimensional system, the
#' values of the derivative of the first dependent derivative at all evaluted
#' points.} \item{dy}{A matrix. In the case of a two dimensional system, the
#' values of the derivative of the second dependent variable at all evaluated
#' points. In the case of a one dimensional system, the values of the
#' derivative of the dependent variable at all evaluated points.}
#' \item{parameters}{As per input.} \item{points}{As per input.}
#' \item{system}{As per input.} \item{x}{A vector. In the case of a two
#' dimensional system, the values of the first dependent variable at which the
#' derivatives were computed. In the case of a one dimensional system, the
#' values of the independent variable at which the derivatives were computed.}
#' \item{xlim}{As per input.} \item{y}{A vector. In the case of a two
#' dimensional system, the of values of the second dependent variable at which
#' the derivatives were computed. In the case of a one dimensional system, the
#' values of the dependent variable at which the derivatives were computed.}
#' \item{ylim}{As per input.}
#' @note In order to ensure a nullclines is plotted, set xlim and ylim
#' strictly enclosing its location. For example, to ensure a nullcline is
#' plotted along x = 0, set ylim to e.g. begin at -1.
#' @author Michael J. Grayling
#' @seealso \code{\link{contour}}, \code{\link{plot}}
#' @export
#' @examples
#' # Plot the flow field, nullclines and several trajectories for the one
#' # dimensional autonomous ODE system logistic.
#' logistic.flowField  <- flowField(logistic, xlim = c(0, 5), ylim = c(-1, 3),
#'                                  parameters = c(1, 2), points = 21, system = "one.dim",
#' 								 add = FALSE)
#' logistic.nullclines <- nullclines(logistic, xlim = c(0, 5), ylim = c(-1, 3),
#'                                   parameters = c(1, 2), system = "one.dim")
#' logistic.trajectory <- trajectory(logistic, y0 = c(-0.5, 0.5, 1.5, 2.5), tlim = c(0, 5),
#'                                   parameters = c(1, 2), system = "one.dim")
#' 
#' # Plot the velocity field, nullclines and several trajectories for the two dimensional
#' # autonomous ODE system simplePendulum.
#' simplePendulum.flowField  <- flowField(simplePendulum, xlim = c(-7, 7),
#'                                        ylim = c(-7, 7), parameters = 5, points = 19,
#' 									   add = FALSE)
#' y0                        <- matrix(c(0, 1, 0, 4, -6, 1, 5, 0.5, 0, -3), ncol = 2,
#'                                     nrow = 5, byrow = TRUE)
#' simplePendulum.nullclines <- nullclines(simplePendulum, xlim = c(-7, 7),
#'                                         ylim = c(-7, 7), parameters = 5, points = 500)
#' simplePendulum.trajectory <- trajectory(simplePendulum, y0 = y0, tlim = c(0, 10),
#'                                         parameters = 5)
#' 
nullclines <- function(deriv, xlim, ylim, parameters = NULL,
                       system = "two.dim", points = 101,
                       col = c("blue", "cyan"), add = TRUE, add.legend = TRUE, 
                       state.names = c("x", "y"), ...){
  if ((!is.vector(xlim)) | (length(xlim) != 2)){
    stop("xlim is not a vector of length 2 as required")
  }
  if (xlim[2] <= xlim[1]){
    stop("xlim[2] is less than or equal to xlim[1]")
  }
  if ((!is.vector(ylim)) | (length(ylim) != 2)){
    stop("ylim is not a vector of length 2 as required")
  }
  if (ylim[2] <= ylim[1]){
    stop("ylim[2] is less than or equal to ylim[1]")
  }
  if (points <= 0) {
    stop("points is less than or equal to zero")
  }
  if (!(system %in% c("one.dim", "two.dim"))){
    stop("system must either be set to \"one.dim\" or \"two.dim\"")
  }
  if (is.vector(col) == FALSE){
    stop("col is not a vector as required")
  }
  if (length(col) != 2){
    if (length(col) == 1){
      col <- rep(col, 2)
    }
    if (length(col) > 2){
      col <- col[1:2]
    }
    print("Note: col has been reset as required")
  }
  if (!is.logical(add)){
    stop("add must be logical")
  }
  if (!is.logical(add.legend)){
    stop("add.legend must be logical")
  }
  x  <- seq(from = xlim[1], to = xlim[2], length = points)
  y  <- seq(from = ylim[1], to = ylim[2], length = points)
  dx <- matrix(0, ncol = points, nrow = points)
  dy <- matrix(0, ncol = points, nrow = points)
  if (system == "one.dim"){
    for (i in 1:points){
      dy[1, i] <- deriv(0, setNames(c(y[i]), state.names[1]), parameters)[[1]]
    }
    for (i in 2:points){
      dy[i, ]  <- dy[1, ]
    }
    contour(x, y, dy, levels = 0, add = add, col = col[1], 
            drawlabels = FALSE, ...)
    if (add.legend == TRUE){
      legend("bottomright", "dy/dt = 0 for all t", lty = 1, lwd = 1,
             col = col[1])
    }
    return(list(add = add, add.legend = add.legend, col = col, deriv = deriv,
                dy = dy, parameters = parameters, points = points,
                system = system, x = x, xlim = xlim, y = y, ylim = ylim))
  } else {
    for (i in 1:points){
      for (j in 1:points){
        df       <- deriv(0, setNames(c(x[i], y[j]), state.names), parameters)
        dx[i, j] <- df[[1]][1]
        dy[i, j] <- df[[1]][2]
      }
    }
    contour(x, y, dx, levels = 0, add = add, col = col[1], 
            drawlabels = FALSE, ...)
    contour(x, y, dy, levels = 0, add = TRUE, col = col[2], 
            drawlabels = FALSE, ...)
    if (add.legend == TRUE){
      legend("bottomright", c("x nullclines", "y nullclines"), lty = 1,
             lwd = 1, col = col)
    }
    return(list(add = add, add.legend = add.legend, col = col, deriv = deriv,
                dx = dx, dy = dy, parameters = parameters, points = points,
                system = system, x = x, xlim = xlim, y = y, ylim = ylim))
  }
}
