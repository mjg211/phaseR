#' Flow Field
#' 
#' Plots the flow or velocity field for a one or two dimensional autonomous ODE
#' system.
#' 
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required format of these functions
#' can be found in the package guide.
#' @param xlim In the case of a two dimensional system, this sets the limits of
#' the first dependent variable in which gradient reflecting line segments
#' should be plotted. In the case of a one dimensional system, this sets the
#' limits of the independent variable in which these line segments should be
#' plotted. Should be a vector of length two.
#' @param ylim In the case of a two dimensional system this sets the limits of
#' the second dependent variable in which gradient reflecting line segments
#' should be plotted. In the case of a one variable system, this sets the
#' limits of the dependent variable in which these line segments should be
#' plotted. Should be a vector of length two.
#' @param parameters Parameters of the ODE system, to be passed to deriv.
#' Supplied as a vector; the order of the parameters can be found from the
#' deriv file. Defaults to NULL.
#' @param points Sets the density of the line segments to be plotted. points
#' segments will be plotted in the x and y directions. Fine tuning here, by
#' shifting points up and down, allows for the creation of more aesthetically
#' pleasing plots. Defaults to 11.
#' @param system Set to either "one.dim" or "two.dim" to indicate the type of
#' system being analysed. Defaults to "two.dim".
#' @param col Sets the colour of the plotted line segments. Should be a vector
#' of length one. Will be reset accordingly if it is a vector of the wrong
#' length. Defaults to "gray".
#' @param arrow.type Sets the type of line segments plotted. If set to
#' "proportional" the length of the line segments reflects the magnitude of the
#' derivative. If set to "equal" the line segments take equal lengths, simply
#' reflecting the gradient of the derivative(s). Defaults to "equal".
#' @param arrow.head Sets the length of the arrow heads. Passed to arrows.
#' Defaults to 0.05.
#' @param frac Sets the fraction of the theoretical maximum length line
#' segments can take without overlapping, that they can actually attain. In
#' practice, frac can be set to greater than 1 without line segments
#' overlapping. Fine tuning here assists the creation of aesthetically pleasing
#' plots. Defaults to 1.
#' @param add Logical. If TRUE, the flow field is added to an existing plot. If
#' FALSE, a new plot is created. Defaults to TRUE.
#' @param xlab Label for the x-axis of the resulting plot.
#' @param ylab Label for the y-axis of the resulting plot.
#' @param \dots Additional arguments to be passed to either plot or arrows.
#' @inheritParams .paramDummy
#' 
#' @return Returns a list with the following components (the exact make up is
#' dependent upon the value of system): \item{add}{As per input.}
#' \item{arrow.head}{As per input.} \item{arrow.type}{As per input.}
#' \item{col}{As per input, but with possible editing if a vector of the wrong
#' length was supplied.} \item{deriv}{As per input.} \item{dx}{A matrix. In the
#' case of a two dimensional system, the values of the derivative of the first
#' dependent derivative at all evaluated points.} \item{dy}{A matrix. In the
#' case of a two dimensional system, the values of the derivative of the second
#' dependent variable at all evaluated points. In the case of a one dimensional
#' system, the values of the derivative of the dependent variable at all
#' evaluated points.} \item{frac}{As per input.} \item{parameters}{As per
#' input.} \item{points}{As per input.} \item{system}{As per input.} \item{x}{A
#' vector. In the case of a two dimensional system, the values of the first
#' dependent variable at which the derivatives were computed. In the case of a
#' one dimensional system, the values of the independent variable at which the
#' derivatives were computed.} \item{xlab}{As per input.} \item{xlim}{As per
#' input.} \item{y}{A vector. In the case of a two dimensional system, the
#' values of the second dependent variable at which the derivatives were
#' computed. In the case of a one dimensional system, the values of the
#' dependent variable at which the derivatives were computed.} \item{ylab}{As
#' per input.} \item{ylim}{As per input.}
#' @author Michael J. Grayling
#' @seealso \code{\link{arrows}}, \code{\link{plot}}
#' @export
#' @examples
#' # Plot the flow field, nullclines and several trajectories for the one
#' # dimensional autonomous ODE system logistic.
#' logistic.flowField <- flowField(logistic, xlim = c(0, 5), ylim = c(-1, 3),
#'                                 parameters = c(1, 2), points = 21, system = "one.dim",
#' 								add = FALSE)
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
flowField <- function(deriv, xlim, ylim, parameters = NULL, system = "two.dim",
                      points = 21, col = "gray", arrow.type = "equal",
                      arrow.head = 0.05, frac = 1, add = TRUE, 
                      state.names = if(system == "two.dim") c("x", "y") else "y", 
                      xlab = if(system == "two.dim") state.names[1] else "t",
                      ylab = if(system == "two.dim") state.names[2] else state.names[1], ...){
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
  if (length(col) > 1){
    col <- col[1]
    print("Note: col has been reset as required")
  }
  if (!(arrow.type %in% c("proportional", "equal"))){
    stop("arrow.type must either be set to \"proportional\" or \"equal\"")
  }
  if (arrow.head <= 0){
    stop("arrow.head is less than or equal to zero")
  }
  if (frac <= 0){
    stop("frac is less than or equal to zero")
  }
  if (!is.logical(add)){
    stop("add must be logical")
  }
  x  <- seq(from = xlim[1], to = xlim[2], length = points)
  y  <- seq(from = ylim[1], to = ylim[2], length = points)
  dx <- matrix(0, ncol = points, nrow = points)
  dy <- matrix(0, ncol = points, nrow = points)
  xmax.length <- x[2] - x[1]
  ymax.length <- y[2] - y[1]
  if (add == FALSE){
    plot(1, xlim = c(xlim[1] - xmax.length, xlim[2] + xmax.length),
	       ylim = c(ylim[1] - ymax.length, ylim[2] + ymax.length),
		     type = "n", xlab = xlab, ylab = ylab, ...)
  }
  if (system == "one.dim"){
    for (i in 1:points){
      dy[1, i] <- deriv(0, setNames(c(y[i]), state.names), parameters)[[1]]
    }
    for (i in 2:points){
      dy[i, ]   <- dy[1, ]
    }
    abs.dy      <- abs(dy)
    abs.dy.non  <- abs.dy[which(abs.dy != 0)]
    max.abs.dy  <- max(abs(dy))
    coefficient <- frac*min(xmax.length, ymax.length)/
                     (2*sqrt(2)*max(sqrt(2*abs.dy.non/
	    			                              (abs.dy.non + (1/abs.dy.non))),
					                          sqrt(2*(1/abs.dy.non)/
						                               (abs.dy.non + (1/abs.dy.non)))))
    for (i in 1:points){
      for (j in 1:points){
        if (dy[i, j] != 0){
          factor    <- sqrt(2/(abs.dy[i, j] + (1/abs.dy[i, j])))
          y.shift   <- coefficient*factor*sqrt(abs.dy[i, j])
          x.shift   <- coefficient*factor/sqrt(abs.dy[i, j])
          if (dy[i, j] < 0){
            y.shift <- -y.shift
          }
        }
        if (dy[i, j] == 0){
          y.shift <- 0
          x.shift <- coefficient*sqrt(2)
        }
        if (arrow.type == "proportional"){
          if (dy[i, j] != 0){
            prop    <- abs.dy[i, j]/max.abs.dy
            y.shift <- y.shift*prop
            x.shift <- x.shift*prop
          }
          if (dy[i, j] == 0) {
            x.shift <- y.shift*mean(abs.dy)/max.abs.dy
          }
        }
        arrows(x[i] - x.shift, y[j] - y.shift, x[i] + x.shift,
		           y[j] + y.shift, length = arrow.head, 
               col = col, ...)
      }
    }
    return(list(add = add, arrow.head = arrow.head, arrow.type = arrow.type, col = col,
                deriv = deriv, dy = dy, frac = frac, parameters = parameters,
                points = points, system = system, x = x, xlab = xlab, xlim = xlim,
                y = y, ylab = ylab, ylim = ylim))
  } else {
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        df       <- deriv(0, setNames(c(x[i], y[j]), state.names), parameters)
        dx[i, j] <- df[[1]][1]
        dy[i, j] <- df[[1]][2]
      }
    }
    abs.dx      <- abs(dx)
    abs.dy      <- abs(dy)
    abs.dx.non  <- abs.dx[which((abs.dx != 0) & (abs.dy != 0))]
    abs.dy.non  <- abs.dy[which((abs.dx != 0) & (abs.dy != 0))]
    max.length  <- max(sqrt(dx^2 + dy^2))
    coefficient <- frac*min(xmax.length, ymax.length)/
                     (2*sqrt(2)*max(sqrt(2*(abs.dy.non/abs.dx.non)/
                                           ((abs.dy.non/abs.dx.non) +
                                           (abs.dx.non/abs.dy.non))),
                                    sqrt(2*(abs.dx.non/abs.dy.non)/
                                           ((abs.dy.non/abs.dx.non) +
                                           (abs.dx.non/abs.dy.non)))))
    for (i in 1:points){
      for (j in 1:points){
        if ((dx[i, j] != 0) | (dy[i, j] != 0)){
          if ((dx[i, j] != 0) & (dy[i, j] != 0)){
            factor    <- sqrt(2/((abs.dy[i, j]/abs.dx[i, j]) +
                                   (abs.dx[i, j]/abs.dy[i, j])))
            y.shift   <- coefficient*factor*sqrt(abs.dy[i, j]/abs.dx[i, j])
            x.shift   <- coefficient*factor/sqrt(abs.dy[i, j]/abs.dx[i, j])
            if (dy[i, j] < 0){
              y.shift <- -abs(y.shift)
            }
            if (dx[i, j] < 0){
              x.shift <- -abs(x.shift)
            }
          }
          if ((dx[i, j] == 0) & (dy[i, j] != 0)){
            y.shift   <- coefficient*sqrt(2)
            x.shift   <- 0
            if (dy[i, j] < 0){
              y.shift <- -abs(y.shift)
            }
          }
          if ((dx[i, j] != 0) & (dy[i, j] == 0)){
            y.shift   <- 0
            x.shift   <- coefficient*sqrt(2)
            if (dx[i, j] < 0){
              x.shift <- -abs(x.shift)
            }
          }
          if (arrow.type == "proportional"){
            prop    <- sqrt((abs.dx[i, j]^2 + abs.dy[i, j]^2))/max.length
            y.shift <- y.shift*prop
            x.shift <- x.shift*prop
          }
          arrows(x[i] - x.shift, y[j] - y.shift, x[i] + x.shift,
                 y[j] + y.shift, length = arrow.head, 
                 col = col, ...)
        }
      }
    }
  }
  return(list(add = add, arrow.head = arrow.head, arrow.type = arrow.type,
              col = col, deriv = deriv, dx = dx, dy = dy, frac = frac,
              parameters = parameters, points = points, system = system,
              x = x, xlab = xlab, xlim = xlim, y = y, ylab = ylab, ylim = ylim))
}
