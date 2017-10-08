findEquilibrium <- function(deriv, y0 = NULL, parameters = NULL,
                            system = "two.dim", tol = 1e-16,
                            max.iter = 50, h = 1e-6, plot.it = FALSE,   
                            summary = TRUE){
  if (is.null(y0)){
    y0   <- locator(n = 1)
    if (system == "one.dim"){
      y0 <- y0$y
    } else {
      y0 <- c(y0$x, y0$y)
    }
  }
  if ((!is.vector(y0)) & (!is.matrix(y0))){
    stop("y0 is not a vector or matrix as required")
  }
  if (is.vector(y0)){
    y0   <- as.matrix(y0)
  }
  if (!(system %in% c("one.dim", "two.dim"))){
    stop("system must either be set to one.dim or two.dim")
  }
  if ((system == "one.dim") & (nrow(y0)*ncol(y0) != 1)){
    stop("For system = \"one.dim\" y0 should be a matrix where nrow(y0)*ncol(y0) = 1 or a vector of length one")
  }
  if ((system == "two.dim") & (nrow(y0)*ncol(y0) != 2)){
    stop("For system = \"two.dim\" y0 should be a matrix where nrow(y0)*ncol(y0) = 2 or a vector of length two")
  }
  if (nrow(y0) < ncol(y0)){
    y0 <- transpose(y0)
  }
  if (tol <= 0){
    stop("tol is less than or equal to zero")
  }
  if (max.iter <= 0){
    stop("max.iter is less than or equal to zero")
  }
  if (h <= 0){
    stop("h is less than or equal to zero")
  }
  if (!is.logical(plot.it)){
    stop("plot.it must either be set to TRUE or FALSE")
  }
  if (!is.logical(summary)){
    stop("summary must either be set to TRUE or FALSE")
  }
  y      <- y0
  dim    <- nrow(y)
  for (i in 1:max.iter){  
    dy       <- deriv(0, y, parameters)[[1]] 
    jacobian <- matrix(0, dim, dim)
    for (j in 1:dim){
      h.vec          <- numeric(dim) 
      h.vec[j]       <- h
      jacobian[, j]  <- (deriv(0, y + h.vec, parameters)[[1]] - deriv(0, y - h.vec, parameters)[[1]])/(2*h) 
    }
    if (sum(dy^2) < tol){ 
      if (system == "one.dim"){
        discriminant     <- jacobian
        if (discriminant > 0){
          classification <- "Unstable"
        }
        if (discriminant < 0){
          classification <- "Stable"
        }
        if (discriminant == 0){
          classification <- "Indeterminate"
        }
      } else {
        A            <- jacobian[1, 1]
        B            <- jacobian[1, 2]
        C            <- jacobian[2, 1]
        D            <- jacobian[2, 2]
        Delta        <- A*D - B*C
        tr           <- A + B
        discriminant <- tr^2 - 4*Delta
        if (Delta < 0){
          classification <- "Saddle"
        }
        if (Delta == 0){
          classification <- "Indeterminate"
        }
        if (Delta > 0){
          if (discriminant > 0){
            if (tr < 0){
              classification <- "Stable node"
            }
            if (tr > 0){
              classification <- "Unstable node"
            }
          }
          if (discriminant < 0){
            if (tr < 0) {
              classification <- "Stable focus"
            }
            if (tr > 0){
              classification <- "Unstable focus"
            }
            if (tr == 0){
              classification <- "Centre"
            }
          }
        }
      }
      if (plot.it == TRUE){
        eigenvalues <- eigen(jacobian)$values 
        pchs        <- matrix(c(17, 5, 2, 16, 1, 1), 2, 3, byrow = TRUE) 
        pch1        <- 1 + as.numeric(Im(eigenvalues[1]) != 0)
        pch2        <- 1 + sum(Re(eigenvalues) > 0)
        par(xpd = TRUE)
        if (system == "one.dim"){
          points(0, y[1], type = "p", pch = pchs[pch1, pch2], cex = 1.5, lwd = 2)
        } else {
          points(y[1], y[2], type = "p", pch = pchs[pch1, pch2], cex = 1.5,
                 lwd = 2)
        }
        par(xpd = FALSE) 
      }
      if (summary == TRUE){
        if (system == "one.dim"){
          cat("Fixed point at y = ", y, "\n")
          cat("\nDiscriminant:", discriminant, "  Classification:", 
              classification)
        } else {
          cat("Fixed point at (x,y) = ", y, "\n")
          cat("\nT:", tr, "  Delta:", Delta, "  Discriminant:", 
              discriminant, "  Classification:", classification)
        }
        cat("\n\n")
      }
      if (system == "one.dim"){
        return(list(classification = classification, deriv = deriv,
                    discriminant = discriminant, h = h, max.iter = max.iter,
                    parameters = parameters, plot.it = plot.it,
                    summary = summary, system = system, tol = tol, y0 = y0,
                    ystar = y))
      } else {
        return(list(classification = classification, Delta = Delta,
                    deriv = deriv, discriminant = discriminant,
                    eigenvalues = eigen(jacobian)$values,
                    eigenvectors = eigen(jacobian)$vectors, h = h,
                    jacobian = jacobian, max.iter = max.iter,
                    parameters = parameters, plot.it = plot.it,
                    summary = summary, system = system, tol = tol, tr = tr,
                    y0 = y0, ystar = y))
      }
    }  
    y <- y - solve(jacobian, dy)
    if (summary == TRUE){
      cat(i, y, "\n")
    }
  }
  if (summary == TRUE){
    cat("Convergence failed")
  }
}