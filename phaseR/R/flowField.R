flowField <- function(deriv, xlim, ylim, parameters = NULL, system = "two.dim",
                      points = 21, col = "gray", arrow.type = "equal",
                      arrow.head = 0.05, frac = 1, add = TRUE, xlab = "x",
                      ylab = "y", ...){
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
      dy[1, i]  <- deriv(t = 0, y = y[i], parameters = parameters)[[1]]
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
        df       <- deriv(t = 0, y = c(x[i], y[j]), parameters = parameters)
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