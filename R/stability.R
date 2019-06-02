#' Stability analysis
#'
#' Uses stability analysis to classify equilibrium points. Uses the Taylor
#' Series approach (also known as perturbation analysis) to classify equilibrium
#' points of a one -imensional autonomous ODE system, or the Jacobian approach
#' to classify equilibrium points of a two-dimensional autonomous ODE system. In
#' addition, it can be used to return the Jacobian at any point of a
#' two-dimensional system.
#'
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required structure of these
#' functions can be found in the package vignette, or in the help file for the
#' function \code{\link[deSolve]{ode}}.
#' @param ystar The point at which to perform stability analysis. For a
#' one-dimensional system this should be a \code{\link[base]{numeric}}
#' \code{\link[base]{vector}} of \code{\link[base]{length}} one, for a
#' two-dimensional system this should be a \code{\link[base]{numeric}}
#' \code{\link[base]{vector}} of \code{\link[base]{length}} two (i.e., presently
#' only one equilibrium point's stability can be evaluated at a time).
#' Alternatively this can be specified as \code{\link[base]{NULL}}, and then
#' \code{\link[graphics]{locator}} can be used to choose a point to perform the
#' analysis for. However, given you are unlikely to locate exactly the equilibrium
#' point, if possible enter \code{ystar} yourself. Defaults to \code{NULL}.
#' @param parameters Parameters of the ODE system, to be passed to \code{deriv}.
#' Supplied as a \code{\link[base]{numeric}} \code{\link[base]{vector}}; the
#' order of the parameters can be found from the \code{deriv} file. Defaults to
#' \code{NULL}.
#' @param system Set to either \code{"one.dim"} or \code{"two.dim"} to indicate
#' the type of system being analysed. Defaults to \code{"two.dim"}.
#' @param h Step length used to approximate the derivative(s). Defaults to
#' \code{1e-7}.
#' @param summary Set to either \code{TRUE} or \code{FALSE} to determine whether
#' a summary of the stability analysis is returned. Defaults to \code{TRUE}.
#' @inheritParams .paramDummy
#' @return Returns a \code{\link[base]{list}} with the following components (the
#' exact make up is dependent upon the value of system):
#' \item{classification}{The classification of \code{ystar}.}
#' \item{Delta}{In the two-dimensional system case, the value of the Jacobian's
#' determinant at \code{ystar}.}
#' \item{deriv}{As per input.}
#' \item{discriminant}{In the one-dimensional system case, the value of the
#' discriminant used in perturbation analysis to assess stability. In the
#' two-dimensional system case, the value of \code{tr^2 - 4*Delta}.}
#' \item{eigenvalues}{In the two-dimensional system case, the value of the
#' Jacobian's eigenvalues at \code{ystar}.}
#' \item{eigenvectors}{In the two-dimensional system case, the value of the
#' Jacobian's eigenvectors at \code{ystar}.}
#' \item{jacobian}{In the two-dimensional system case, the Jacobian at
#' \code{ystar}.}
#' \item{h}{As per input.}
#' \item{parameters}{As per input.}
#' \item{summary}{As per input.}
#' \item{system}{As per input.}
#' \item{tr}{In the two-dimensional system case, the value of the Jacobian's
#' trace at \code{ystar}.}
#' \item{ystar}{As per input.}
#' @author Michael J Grayling
#' @export
#' @examples
#' # Determine the stability of the equilibrium points of the one-dimensional
#' # autonomous ODE system example2
#' example2_stability_1 <- stability(example2, ystar = 0, system = "one.dim")
#' example2_stability_2 <- stability(example2, ystar = 1, system = "one.dim")
#' example2_stability_3 <- stability(example2, ystar = 2, system = "one.dim")
#'
#' # Determine the stability of the equilibrium points of the two-dimensional
#' # autonomous ODE system example11
#' example11_stability_1 <- stability(example11, ystar = c(0, 0))
#' example11_stability_2 <- stability(example11, ystar = c(0, 2))
#' example11_stability_3 <- stability(example11, ystar = c(1, 1))
#' example11_stability_4 <- stability(example11, ystar = c(3, 0))
stability <- function(deriv, ystar = NULL, parameters = NULL,
                      system = "two.dim", h = 1e-07, summary = TRUE,
                      state.names =
                        if (system == "two.dim") c("x", "y") else "y") {
  if (is.null(ystar)) {
    ystar                <- locator(n = 1)
    if (system == "two.dim") {
      ystar              <- c(ystar$x, ystar$y)
    } else {
      ystar              <- ystar$y
    }
  }
  if (!(system %in% c("one.dim", "two.dim"))) {
    stop("system must be set to either \"one.dim\" or \"two.dim\"")
  }
  if (all(!is.vector(ystar), !is.matrix(ystar))) {
    stop("ystar is not a vector or matrix as required")
  }
  if (is.vector(ystar)) {
    ystar                <- as.matrix(ystar)
  }
  if (all(system == "one.dim", nrow(ystar)*ncol(ystar) != 1)) {
    stop("For system = \"one.dim\", ystar should be a matrix where ",
         "nrow(ystar)*ncol(ystar) = 1 or a vector of length one")
  }
  if (all(system == "two.dim", nrow(ystar)*ncol(ystar) != 2)) {
    stop("For system = \"two.dim\", ystar should be a matrix where ",
         "nrow(ystar)*ncol(ystar) = 2 or a vector of length two")
  }
  if (nrow(ystar) < ncol(ystar)) {
    ystar                <- t(ystar)
  }
  if (h <= 0) {
    stop("h is less than or equal to zero")
  }
  if (!is.logical(summary)) {
    stop("summary must either be set to either TRUE or FALSE")
  }
  if (system == "one.dim"){
    discriminant         <- as.numeric(
      (deriv(0, stats::setNames(ystar + h, state.names),
             parameters = parameters)[[1]] -
         deriv(0, stats::setNames(ystar - h, state.names),
               parameters = parameters)[[1]])/(2*h))
    if (discriminant > 0) {
      classification     <- "Unstable"
    } else if (discriminant < 0) {
      classification     <- "Stable"
    } else {
      classification     <- "Indeterminate"
    }
    if (summary) {
      message("discriminant = ", round(discriminant, 5), ", classification = ",
              classification)
    }
    return(list(classification = classification,
                deriv          = deriv,
                discriminant   = discriminant,
                h              = h,
                parameters     = parameters,
                summary        = summary,
                system         = system,
                ystar          = ystar))
  } else {
    jacobian             <- matrix(0, 2, 2)
    for (j in 1:2) {
      h.vec              <- numeric(2)
      h.vec[j]           <- h
      jacobian[, j]      <-
        (deriv(0, stats::setNames(ystar + h.vec, state.names), parameters)[[1]] -
           deriv(0, stats::setNames(ystar - h.vec, state.names),
                 parameters)[[1]])/(2*h)
    }
    A                    <- jacobian[1, 1]
    B                    <- jacobian[1, 2]
    C                    <- jacobian[2, 1]
    D                    <- jacobian[2, 2]
    Delta                <- A*D - B*C
    tr                   <- A + D
    discriminant         <- tr^2 - 4*Delta
    eigen                <- eigen(jacobian)
    eigenvalues          <- eigen$values
    eigenvectors         <- eigen$vectors
    if (Delta < 0) {
      classification     <- "Saddle"
    } else if (Delta == 0) {
      classification     <- "Indeterminate"
    } else {
      if (discriminant > 0) {
        if (tr < 0) {
          classification <- "Stable node"
        } else if (tr > 0) {
          classification <- "Unstable node"
        }
      } else if (discriminant < 0) {
        if (tr < 0) {
          classification <- "Stable focus"
        } else if (tr > 0) {
          classification <- "Unstable focus"
        } else {
          classification <- "Centre"
        }
      }
    }
    if (summary) {
      message("tr = ", round(tr, 5), ", Delta = ", round(Delta, 5),
              ", discriminant = ",  round(discriminant, 5),
              ", classification = ", classification)
    }
    return(list(classification = classification,
                Delta          = Delta,
                deriv          = deriv,
                discriminant   = discriminant,
                eigenvalues    = eigenvalues,
                eigenvectors   = eigenvectors,
                h              = h,
                jacobian       = jacobian,
                parameters     = parameters,
                summary        = summary,
                system         = system,
                tr             = tr,
                ystar          = ystar))
  }
}
