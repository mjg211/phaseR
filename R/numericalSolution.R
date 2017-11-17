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