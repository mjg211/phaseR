stability <- function(deriv, ystar = NULL, parameters = NULL,
                      system = "two.dim", h = 1e-07, summary = TRUE){
  if (is.null(ystar)){
    ystar   <- locator(n = 1)
    if (system == "two.dim"){
      ystar <- c(ystar$x, ystar$y)
    }
    if (system == "one.dim"){
      ystar <- ystar$y
    }
  }
  if (!(system %in% c("one.dim", "two.dim"))){
    stop("system must either be set to \"one.dim\" or \"two.dim\"")
  }
  if ((!is.vector(ystar)) & (!is.matrix(y0))){
    stop("ystar is not a vector or matrix as required")
  }
  if (is.vector(ystar)){
    ystar <- as.matrix(ystar)
  }
  if ((system == "one.dim") & (nrow(ystar)*ncol(ystar) != 1)){
    stop("For system = \"one.dim\" ystar should be a matrix where nrow(ystar)*ncol(ystar) = 1 or a vector of length one")
  }
  if ((system == "two.dim") & (nrow(ystar)*ncol(ystar) != 2)){
    stop("For system = \"two.dim\" ystar should be a matrix where nrow(ystar)*ncol(ystar) = 2 or a vector of length two")
  }
  if (nrow(ystar) < ncol(ystar)){
    ystar <- transpose(ystar)
  }
  if (h <= 0){
    stop("h is less than or equal to zero")
  }
  if (!is.logical(summary)){
    stop("summary must either be set to TRUE or FALSE")
  }
  if (system == "one.dim"){
    discriminant     <- (deriv(0, ystar + h, parameters = parameters)[[1]] -
                           deriv(0, ystar - h, parameters = parameters)[[1]])/
                          (2*h)
    if (discriminant > 0){
      classification <- "Unstable"
    }
    if (discriminant < 0){
      classification <- "Stable"
    }
    if (discriminant == 0){
      classification <- "Indeterminate"
    }
    if (summary == TRUE){
      cat("Discriminant:", discriminant, "  Classification:", 
          classification)
    }
    return(list(classification = classification, deriv = deriv,
                discriminant = discriminant, h = h, parameters = parameters,
                summary = summary, system = system, ystar = ystar))
  } else {
    jacobian        <- matrix(0, 2, 2)
    for (j in 1:2){
      h.vec         <- numeric(2) 
      h.vec[j]      <- h
      jacobian[, j] <- (deriv(0, ystar + h.vec, parameters)[[1]] -
                          deriv(0, ystar - h.vec, parameters)[[1]])/(2*h) 
    }
    A            <- jacobian[1, 1]
    B            <- jacobian[1, 2]
    C            <- jacobian[2, 1]
    D            <- jacobian[2, 2]
    Delta        <- A*D - B*C
    tr           <- A + D
    discriminant <- tr^2 - 4*Delta
    eigen        <- eigen(jacobian)
    eigenvalues  <- eigen$values
    eigenvectors <- eigen$vectors
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
    if (summary == TRUE){
      cat("T:", tr, "  Delta:", Delta, "  Discriminant:", 
          discriminant, "  Classification:", classification)
    }
    return(list(classification = classification, Delta = Delta, deriv = deriv,
                discriminant = discriminant, eigenvalues = eigenvalues,
                eigenvectors = eigenvectors, h = h, jacobian = jacobian,
                parameters = parameters, summary = summary, system = system,
                tr = tr, ystar = ystar))
  }
}