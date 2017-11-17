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