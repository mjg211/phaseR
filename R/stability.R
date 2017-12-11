#' Stability Analysis
#' 
#' Uses stability analysis to classify equilibrium points. Uses the Taylor
#' Series approach (also known as Perturbation Analysis) to classify
#' equilibrium points of a one dimensional autonomous ODE system, or the
#' Jacobian approach to classify equilibrium points of a two dimensional
#' autonomous ODE system. In addition, it can be used to return the Jacobian at
#' any point of a two dimensional system.
#' 
#' 
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required structure of these
#' functions can be found in the package guide.
#' @param ystar The point at which to perform stability analysis. For a one
#' variable system this should be a single number, for a two variable system
#' this should be a vector of length two (i.e. presently only one equilibrium
#' points stability can be evaluated at a time). Alternatively this can be left
#' blank and the user can use locator to choose a point to perform the
#' analysis. However, given you are unlikely to locate exactly the equilibrium
#' point, if possible enter y.star yourself. Defaults to NULL.
#' @param parameters Parameters of the ODE system, to be passed to deriv.
#' Supplied as a vector; the order of the parameters can be found from the
#' deriv file. Defaults to NULL.
#' @param system Set to either "one.dim" or "two.dim" to indicate the type of
#' system being analysed. Defaults to "two.dim".
#' @param h Step length used to approximate the derivative(s). Defaults to
#' 1e-7.
#' @param summary Set to either TRUE or FALSE to determine whether a summary of
#' the stability analysis is returned. Defaults to TRUE.
#' @inheritParams .paramDummy
#' 
#' @return Returns a list with the following components (the exact make up is
#' dependent upon the value of system): \item{classification}{The
#' classification of y.star.} \item{Delta}{In the two dimensional system case,
#' value of the Jacobians determinant at y.star.} \item{deriv}{As per input.}
#' \item{discriminant}{In the one dimensional system case, the value of the
#' discriminant used in Perturbation Analysis to assess stability. In the two
#' dimensional system case, the value of T^2 - 4*Delta.} \item{eigenvalues}{In
#' the two dimensional system case, the value of the Jacobians eigenvalues at
#' y.star.} \item{eigenvectors}{In the two dimensional system case, the value
#' of the Jacobians eigenvectors at y.star.} \item{jacobian}{In the two
#' dimensional system case, the Jacobian at y.star.} \item{h}{As per input.}
#' \item{parameters}{As per input.} \item{summary}{As per input.}
#' \item{system}{As per input.} \item{tr}{In the two dimensional system case,
#' the value of the Jacobians trace at y.star.} \item{ystar}{As per input.}
#' @author Michael J. Grayling
#' @export
#' @examples
#' # Determine the stability of the equilibrium points of the one dimensional
#' # autonomous ODE system example2.
#' example2.stability.1 <- stability(example2, ystar = 0, system = "one.dim")
#' example2.stability.2 <- stability(example2, ystar = 1, system = "one.dim")
#' example2.stability.3 <- stability(example2, ystar = 2, system = "one.dim")
#' 
#' # Determine the stability of the equilibrium points of the two dimensional autonomous
#' # ODE system example11.
#' example11.stability.1 <- stability(example11, ystar = c(0, 0))
#' example11.stability.2 <- stability(example11, ystar = c(0, 2))
#' example11.stability.3 <- stability(example11, ystar = c(1, 1))
#' example11.stability.4 <- stability(example11, ystar = c(3, 0))
#' 
stability <- function(deriv, ystar = NULL, parameters = NULL,
                      system = "two.dim", h = 1e-07, summary = TRUE, 
                      state.names = c("x", "y")){
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
  if ((!is.vector(ystar)) & (!is.matrix(ystar))){
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
    discriminant <- as.numeric(
      (deriv(0, setNames(ystar + h, state.names[1]), parameters = parameters)[[1]] 
       - deriv(0, setNames(ystar - h, state.names[1]), parameters = parameters)[[1]])
      / (2*h))
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
      jacobian[, j] <- (deriv(0, setNames(ystar + h.vec, state.names), parameters)[[1]] -
                          deriv(0, setNames(ystar - h.vec, state.names), parameters)[[1]])/(2*h) 
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
