#' Example ODE System Number Nine
#' 
#' The derivative function of an example two dimensional autonomous ODE system.
#' 
#' Evaluates the derivative of the following coupled ODE system at the point
#' (t, x, y):
#' 
#' dx/dt = -2*x + 3*y, dy/dt = 7*x + 6*y.
#' 
#' Format is designed to be compatible with ode from the deSolve package.
#' 
#' @param t Value of t, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y Values of x and y, the dependent variables, to evaluate the
#' derivative at. Should be a vector of length 2.
#' @param parameters Values of the parameters of the system. Not required here.
#' @return Returns a list dy containing the values of the two derivatives at
#' (t, x, y).
#' @author Michael J. Grayling
#' @seealso \code{\link{ode}}
#' @export
#' @examples
#' # Plot the velocity field, nullclines and several trajectories.
#' example9.flowField  <- flowField(example9, xlim = c(-3, 3), ylim = c(-3, 3),
#'                                  points = 19, add = FALSE)
#' y0                  <- matrix(c(1, 0, -3, 2, 2, -2, -2, -2), ncol = 2, nrow = 4,
#'                               byrow = TRUE)
#' example9.nullclines <- nullclines(example9, xlim = c(-3, 3), ylim = c(-3, 3))
#' example9.trajectory <- trajectory(example9, y0 = y0, tlim = c(0, 10))
#' example9.manifolds  <- drawManifolds(example9, y0 = c(0, 0), tend = 1000,
#'                                      col = c("green", "red"), add.legend = TRUE)
#' 
#' # Determine the stability of the equilibrium point.
#' example9.stability <- stability(example9, ystar = c(0, 0))
#' 
example9 <- function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  dy    <- numeric(2)
  dy[1] <- -2*x + 3*y
  dy[2] <- 7*x + 6*y
  list(dy)
}
