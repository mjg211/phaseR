#' Phase Portrait Plot
#' 
#' For a one dimensional autonomous ODE, it plots the phase portrait i.e. the
#' derivative against the dependent variable. In addition, along the dependent
#' variable axis it plots arrows pointing in the direction of dependent
#' variable change with increasing value of the independent variable. From this
#' stability of equilibrium points (i.e. locations where the horizontal axis is
#' crossed) can be determined.
#' 
#' 
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required structure of these
#' functions can be found in the package guide.
#' @param ylim Sets the limits of the dependent variable for which the
#' derivative should be computed and plotted. Should be a vector of length two.
#' @param ystep Sets the step length of the dependent variable vector for which
#' derivatives are computed and plotted. Decreasing ystep makes the resulting
#' plot more accurate, but comes at a small cost to computation time. Defaults
#' to 0.01.
#' @param parameters Parameters of the ODE system, to be passed to deriv.
#' Supplied as a vector; the order of the parameters can be found from the
#' deriv file.
#' @param points Sets the density at which arrows are plotted along the
#' horizontal axis. points arrows will be plotted. Fine tuning here, by
#' shifting points up and down, allows for the creation of more aesthetically
#' pleasing plots. Defaults to 10.
#' @param frac Sets the fraction of the theoretical maximum length line
#' segments can take without overlapping, that they actually attain. Fine
#' tuning here assists the creation of aesthetically pleasing plots. Defaults
#' to 0.75.
#' @param arrow.head Sets the length of the arrow heads. Passed to arrows.
#' Defaults to 0.075.
#' @param col Sets the colour of the line in the plot, as well as the arrows.
#' Will be reset accordingly if it is not a vector of length one. Defaults to
#' "black".
#' @param xlab Label for the x-axis of the resulting plot. Defaults to "y".
#' @param ylab Label for the y-axis of the resulting plot. Defaults to "f(y)".
#' @param add.grid Logical. If TRUE, a grid is added to the plot. Defaults to
#' TRUE.
#' @param \dots Additional arguments to be passed to either plot or arrows.
#' @inheritParams .paramDummy
#' 
#' @return Returns a list with the following components: \item{add.grid}{As per
#' input.} \item{arrow.head}{As per input.} \item{col}{As per input.}
#' \item{deriv}{As per input.} \item{dy}{A vector containing the value of the
#' derivative at each evaluated point.} \item{frac}{As per input.}
#' \item{parameters}{As per input.} \item{points}{As per input.} \item{xlab}{As
#' per input.} \item{y}{A vector containing the values of the dependent
#' variable for which the derivative was evaluated.} \item{ylab}{As per input.}
#' \item{ylim}{As per input.} \item{ystep}{As per input.}
#' @author Michael J. Grayling
#' @seealso \code{\link{arrows}}, \code{\link{plot}}
#' @export
#' @examples
#' # A one dimensional autonomous ODE system, example2.
#' example2.phasePortrait <- phasePortrait(example2, ylim = c(-0.5, 2.5), points = 10,
#'                                         frac = 0.5)
#' 
phasePortrait <- function(deriv, ylim, ystep = 0.01, parameters = NULL,
                          points = 10, frac = 0.75, arrow.head = 0.075,
                          col = "black", xlab = "y", ylab = "f(y)",
                          add.grid = TRUE, state.names = c("y"), ...){
    if ((!is.vector(ylim)) | (length(ylim) != 2)){
        stop("ylim is not a vector of length 2 as required")
    }
    if (ylim[2] <= ylim[1]){
        stop("ylim[2] is less than or equal to ylim[1]")
    }
    if (ystep <= 0){
        stop("ystep is less than or equal to zero")
    }
    if (!is.vector(col)){
        stop("col is not a vector as required")
    }
    if (length(col) > 1){
        col <- col[1]
        print("Note: col has been reset as required")
    }
    if (!is.logical(add.grid)){
      stop("add.grid must be set to TRUE or FALSE")
    }
    y       <- seq(from = ylim[1], to = ylim[2], by = ystep)
    dy      <- numeric(length(y))
    for (i in 1:length(y)){
      dy[i] <- deriv(0, setNames(y[i], state.names[1]), parameters)[[1]]
    }
    plot(y, dy, col = col, type = "l", xlab = xlab, ylab = ylab, ...)
    if (add.grid == TRUE){
      grid()
    }
    y.arrows  <- seq(from = ylim[1], to = ylim[2], length = points)
    dy.arrows <- numeric(points)
    y.shift   <- 0.5*frac*(y.arrows[2] - y.arrows[1])
    for (i in 1:points){
      dy.arrows[i] <- deriv(0, setNames(y.arrows[i], state.names[1]),
                            parameters)[[1]]
    }
    pos <- which(dy.arrows > 0)
    arrows(y.arrows[pos] - y.shift, numeric(length(y.arrows[pos])), 
           y.arrows[pos] + y.shift, numeric(length(y.arrows[pos])), 
           length = arrow.head, col = col, ...)
    neg <- which(dy.arrows < 0)
    arrows(y.arrows[neg] + y.shift, numeric(length(y.arrows[neg])), 
           y.arrows[neg] - y.shift, numeric(length(y.arrows[neg])), 
           length = arrow.head, col = col, ...)
    return(list(add.grid = add.grid, arrow.head = arrow.head, col = col,
                deriv = deriv, dy = dy, frac = frac, parameters = parameters,
                points = points, xlab = xlab, y = y, ylab = ylab, ylim = ylim,
                ystep = ystep))
}
