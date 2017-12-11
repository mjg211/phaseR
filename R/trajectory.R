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