#' Equilibrium point identification
#'
#' Searches for an equilibium point of a system, taking the starting point of
#' the search as a user specified location. On identifying such a point, a
#' classification is performed, and an informatively shaped point can be added
#' to the plot.
#'
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required structure of these
#' functions can be found in the package vignette, or in the help file for the
#' function \code{\link[deSolve]{ode}}.
#' @param y0 The starting point of the search. In the case of a one-dimensional
#' system, this should be a \code{\link[base]{numeric}}
#' \code{\link[base]{vector}} of \code{\link[base]{length}} one indicating the
#' location of the dependent variable initially. In the case of a
#' two-dimensional system, this should be a \code{\link[base]{numeric}}
#' \code{\link[base]{vector}} of \code{\link[base]{length}} two reflecting the
#' location of the two dependent variables initially. Alternatively this can be
#' specified as \code{\link[base]{NULL}}, and then
#' \code{\link[graphics]{locator}} can be used to specify the initial point on a
#' plot. Defaults to \code{\link[base]{NULL}}.
#' @param parameters Parameters of the ODE system, to be passed to \code{deriv}.
#' Supplied as a \code{\link[base]{numeric}} \code{\link[base]{vector}}; the
#' order of the parameters can be found from the \code{deriv} file. Defaults to
#' \code{NULL}.
#' @param system Set to either \code{"one.dim"} or \code{"two.dim"} to indicate
#' the type of system being analysed. Defaults to \code{"two.dim"}.
#' @param tol The tolerance for the convergence of the search algorithm.
#' Defaults to \code{1e-16}.
#' @param max.iter The maximum allowed number of iterations of the search
#' algorithm. Defaults to \code{50}.
#' @param h Step length used to approximate the derivative(s). Defaults to
#' \code{1e-6}.
#' @param plot.it Logical. If \code{TRUE}, a point is plotted at the identified
#' equilibrium point, with shape corresponding to its classification.
#' @param summary Set to either \code{TRUE} or \code{FALSE} to determine whether
#' a summary of the progress of the search procedure is returned. Defaults to
#' \code{TRUE}.
#' @inheritParams .paramDummy
#' @return Returns a list with the following components (the exact make up is
#' dependent on the value of \code{system}):
#' \item{classification}{The classification of the identified equilibrium
#' point.}
#' \item{Delta}{In the two-dimensional system case, value of the Jacobian's
#' determinant at the equilibrium point.}
#' \item{deriv}{As per input.}
#' \item{discriminant}{In the one-dimensional system case, the value of the
#' discriminant used in perturbation analysis to assess stability. In the
#' two-dimensional system case, the value of \code{tr^2 - 4*Delta}.}
#' \item{eigenvalues}{In the two-dimensional system case, the value of the
#' Jacobian's eigenvalues at the equilibrium point.}
#' \item{eigenvectors}{In the two-dimensional system case, the value of the
#' Jacobian's eigenvectors at the equilibrium point.}
#' \item{jacobian}{In the two-dimensional system case, the Jacobian at the
#' equilibrium point.}
#' \item{h}{As per input.}
#' \item{max.iter}{As per input.}
#' \item{parameters}{As per input.}
#' \item{plot.it}{As per input.}
#' \item{summary}{As per input.}
#' \item{system}{As per input.}
#' \item{tr}{In the two-dimensional system case, the value of the Jacobian's
#' trace at the equilibrium point.}
#' \item{tol}{As per input.}
#' \item{y0}{As per input.}
#' \item{ystar}{The location of the identified equilibrium point.}
#' @author Michael J Grayling, Stephen P Ellner, John M Guckenheimer
#' @export
findEquilibrium <- function(deriv, y0 = NULL, parameters = NULL,
                            system = "two.dim", tol = 1e-16,
                            max.iter = 50, h = 1e-6, plot.it = FALSE,
                            summary = TRUE,
                            state.names =
                              if (system == "two.dim") c("x", "y") else "y") {
  if (is.null(y0)) {
    y0                       <- locator(n = 1)
    if (system == "one.dim") {
      y0                     <- y0$y
    } else {
      y0                     <- c(y0$x, y0$y)
    }
  }
  if (all(!is.vector(y0), !is.matrix(y0))) {
    stop("y0 is not a vector or matrix, as is required")
  }
  if (is.vector(y0)) {
    y0                       <- as.matrix(y0)
  }
  if (!(system %in% c("one.dim", "two.dim"))) {
    stop("system must be set to either \"one.dim\" or \"two.dim\"")
  }
  if (all(system == "one.dim", nrow(y0)*ncol(y0) != 1)) {
    stop("For system = \"one.dim\", y0 should be a matrix where ",
         "nrow(y0)*ncol(y0) = 1 or a vector of length one")
  }
  if (all(system == "two.dim", nrow(y0)*ncol(y0) != 2)) {
    stop("For system = \"two.dim\", y0 should be a matrix where ",
         "nrow(y0)*ncol(y0) = 2 or a vector of length two")
  }
  if (nrow(y0) < ncol(y0)) {
    y0                       <- t(y0)
  }
  if (tol <= 0) {
    stop("tol is less than or equal to zero")
  }
  if (max.iter <= 0) {
    stop("max.iter is less than or equal to zero")
  }
  if (h <= 0) {
    stop("h is less than or equal to zero")
  }
  if (!is.logical(plot.it)) {
    stop("plot.it must be set to either TRUE or FALSE")
  }
  if (!is.logical(summary)){
    stop("summary must be set to either TRUE or FALSE")
  }
  y                          <- y0
  dim                        <- nrow(y)
  for (i in 1:max.iter) {
    dy                       <-
      deriv(0, stats::setNames(y, utils::head(state.names, n = dim)),
            parameters)[[1]]
    jacobian                 <- matrix(0, dim, dim)
    for (j in 1:dim) {
      h.vec                  <- numeric(dim)
      h.vec[j]               <- h
      jacobian[, j]          <-
        (deriv(0, stats::setNames(y + h.vec, state.names), parameters)[[1]] -
           deriv(0, stats::setNames(y - h.vec, state.names),
                 parameters)[[1]])/(2*h)
    }
    if (sum(dy^2) < tol) {
      if (system == "one.dim") {
        discriminant         <- jacobian
        if (discriminant > 0) {
          classification     <- "Unstable"
        } else if (discriminant < 0) {
          classification     <- "Stable"
        } else {
          classification     <- "Indeterminate"
        }
      } else {
        A                    <- jacobian[1, 1]
        B                    <- jacobian[1, 2]
        C                    <- jacobian[2, 1]
        D                    <- jacobian[2, 2]
        Delta                <- A*D - B*C
        tr                   <- A + D
        discriminant         <- tr^2 - 4*Delta
        if (Delta == 0) {
          classification     <- "Indeterminate"
        } else if (discriminant == 0) {
          if (tr < 0) {
            classification   <- "Stable node"
          } else {
            classification   <- "Unstable node"
          }
        } else if (Delta < 0) {
            classification   <- "Saddle"
        } else {
          if (discriminant > 0) {
            if (tr < 0) {
              classification <- "Stable node"
            } else {
              classification <- "Unstable node"
            }
          } else {
            if (tr < 0) {
              classification <- "Stable focus"
            } else if (tr > 0) {
              classification <- "Unstable focus"
            } else {
              classification <- "Centre"
            }
          }
        }
      }
      if (plot.it) {
        eigenvalues          <- eigen(jacobian)$values
        pchs                 <- matrix(c(17, 5, 2, 16, 1, 1), 2, 3, byrow = T)
        pch1                 <- 1 + as.numeric(Im(eigenvalues[1]) != 0)
        pch2                 <- 1 + sum(Re(eigenvalues) > 0)
        old.par              <- graphics::par(no.readonly = T)
        on.exit(graphics::par(old.par))
        graphics::par(xpd = T)
        if (system == "one.dim") {
          graphics::points(0, y[1], type = "p", pch = pchs[pch1, pch2],
                           cex = 1.5, lwd = 2)
        } else {
          graphics::points(y[1], y[2], type = "p", pch = pchs[pch1, pch2],
                           cex = 1.5, lwd = 2)
        }
      }
      if (summary) {
        if (system == "one.dim") {
          message("Fixed point at ", state.names," = ", round(y, 5))
          message("discriminant = ", round(discriminant, 5),
                  ", classification = ", classification)
        } else {
          message("Fixed point at (", paste0(state.names, collapse = ','),
                  ") = ", round(y, 5))
          message("tr = ", round(tr, 5), ", Delta = ", round(Delta, 5),
                  ", discriminant = ", round(discriminant, 5),
                  ", classification = ", classification)
        }
      }
      if (system == "one.dim") {
        return(list(classification = classification,
                    deriv          = deriv,
                    discriminant   = discriminant,
                    h              = h,
                    max.iter       = max.iter,
                    parameters     = parameters,
                    plot.it        = plot.it,
                    summary        = summary,
                    system         = system,
                    tol            = tol,
                    y0             = y0,
                    ystar          = y))
      } else {
        return(list(classification = classification,
                    Delta          = Delta,
                    deriv          = deriv,
                    discriminant   = discriminant,
                    eigenvalues    = eigen(jacobian)$values,
                    eigenvectors   = eigen(jacobian)$vectors,
                    h              = h,
                    jacobian       = jacobian,
                    max.iter       = max.iter,
                    parameters     = parameters,
                    plot.it        = plot.it,
                    summary        = summary,
                    system         = system,
                    tol            = tol,
                    tr             = tr,
                    y0             = y0,
                    ystar          = y))
      }
    }
    y                        <- y - solve(jacobian, dy)
    if (summary) {
      message(i, y)
    }
  }
  if (summary) {
    message("Convergence failed")
  }
}

findEquilibrium2 <- function(deriv, y0 = NULL, parameters = NULL,
                            system = "two.dim", tol = 1e-16,
                            max.iter = 50, h = 1e-6, plot.it = FALSE,
                            summary = TRUE,
                            state.names =
                              if (system == "two.dim") c("x", "y") else "y") {
  if (is.null(y0)) {
    y0                       <- locator(n = 1)
    if (system == "one.dim") {
      y0                     <- y0$y
    } else {
      y0                     <- c(y0$x, y0$y)
    }
  }
  if (all(!is.vector(y0), !is.matrix(y0))) {
    stop("y0 is not a vector or matrix, as is required")
  }
  if (is.vector(y0)) {
    y0 <- as.matrix(y0)
  }
  if (!(system %in% c("one.dim", "two.dim"))) {
    stop("system must be set to either \"one.dim\" or \"two.dim\"")
  }
  if (all(system == "one.dim", nrow(y0)*ncol(y0) != 1)) {
    stop("For system = \"one.dim\", y0 should be a matrix where ",
         "nrow(y0)*ncol(y0) = 1 or a vector of length one")
  }
  if (all(system == "two.dim", nrow(y0)*ncol(y0) != 2)) {
    stop("For system = \"two.dim\", y0 should be a matrix where ",
         "nrow(y0)*ncol(y0) = 2 or a vector of length two")
  }
  if (nrow(y0) < ncol(y0)) {
    y0 <- t(y0)
  }
  if (tol <= 0) {
    stop("tol is less than or equal to zero")
  }
  if (max.iter <= 0) {
    stop("max.iter is less than or equal to zero")
  }
  if (h <= 0) {
    stop("h is less than or equal to zero")
  }
  if (!is.logical(plot.it)) {
    stop("plot.it must be set to either TRUE or FALSE")
  }
  if (!is.logical(summary)){
    stop("summary must be set to either TRUE or FALSE")
  }
  y <- y0
  dim <- nrow(y)
  for (i in 1:max.iter) {

    dy <- deriv(0, stats::setNames(y, utils::head(state.names, n = dim)), parameters)[[1]]
    jacobian <- matrix(0, dim, dim)

    for (j in 1:dim) {
      h.vec <- numeric(dim)
      h.vec[j] <- h
      jacobian[, j] <- (deriv(0, stats::setNames(y + h.vec, state.names), parameters)[[1]] - deriv(0, stats::setNames(y - h.vec, state.names), parameters)[[1]]) / (2 * h)
    }

    if (sum(dy ^ 2) < tol) {

      if (system == "one.dim") {
        discriminant <- jacobian
        if (discriminant > 0) {
          classification <- "Unstable"
        } else if (discriminant < 0) {
          classification <- "Stable"
        } else {
          classification <- "Indeterminate"
        }
      } else {

        Delta <- jacobian[1, 1] * jacobian[2, 2] - jacobian[1, 2] * jacobian[2, 1]
        tr <- sum(diag(jacobian))
        discriminant <- tr ^ 2 - 4 * Delta

        if (Delta == 0) {
          classification <- "Indeterminate"
        } else if (discriminant == 0) {
          if (tr < 0) {
            classification <- "Stable node"
          } else {
            classification <- "Unstable node"
          }
        } else if (Delta < 0) {
          classification <- "Saddle"
        } else {
          if (discriminant > 0) {
            if (tr < 0) {
              classification <- "Stable node"
            } else {
              classification <- "Unstable node"
            }
          } else {
            if (tr < 0) {
              classification <- "Stable focus"
            } else if (tr > 0) {
              classification <- "Unstable focus"
            } else {
              classification <- "Centre"
            }
          }
        }
      }
      if (plot.it) {
        eigenvalues <- eigen(jacobian)$values
        pchs <- matrix(c(17, 5, 2, 16, 1, 1), 2, 3, byrow = T)
        pch1 <- 1 + as.numeric(Im(eigenvalues[1]) != 0)
        pch2 <- 1 + sum(Re(eigenvalues) > 0)
        old.par  <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(old.par))
        graphics::par(xpd = TRUE)

        if (system == "one.dim") {
          graphics::points(0, y[1], type = "p", pch = pchs[pch1, pch2],
                           cex = 1.5, lwd = 2)
        } else {
          graphics::points(y[1], y[2], type = "p", pch = pchs[pch1, pch2],
                           cex = 1.5, lwd = 2)
        }
      }
      if (summary) {
        if (system == "one.dim") {
          message("Fixed point at ", state.names," = ", round(y, 5))
          message("discriminant = ", round(discriminant, 5), ", classification = ", classification)
        } else {
          message("Fixed point at (", paste0(state.names, collapse = ','), ") = ", round(y, 5))

          message("tr = ", round(tr, 5), ", Delta = ", round(Delta, 5),
                  ", discriminant = ", round(discriminant, 5),
                  ", classification = ", classification)
        }
      }
      if (system == "one.dim") {
        return(list(classification = classification,
                    deriv          = deriv,
                    discriminant   = discriminant,
                    h              = h,
                    max.iter       = max.iter,
                    parameters     = parameters,
                    plot.it        = plot.it,
                    summary        = summary,
                    system         = system,
                    tol            = tol,
                    y0             = y0,
                    ystar          = y))
      } else {
        return(list(classification = classification,
                    Delta          = Delta,
                    deriv          = deriv,
                    discriminant   = discriminant,
                    eigenvalues    = eigen(jacobian)$values,
                    eigenvectors   = eigen(jacobian)$vectors,
                    h              = h,
                    jacobian       = jacobian,
                    max.iter       = max.iter,
                    parameters     = parameters,
                    plot.it        = plot.it,
                    summary        = summary,
                    system         = system,
                    tol            = tol,
                    tr             = tr,
                    y0             = y0,
                    ystar          = y))
      }
    }
    y <- y - solve(jacobian, dy)
    if (summary) {
      message(i, y)
    }
  }
  if (summary) {
    message("Convergence failed")
  }
}
