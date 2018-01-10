#' Example ODE System Number Twelve
#' 
#' The derivative function of an example two dimensional autonomous ODE system.
#' 
#' Evaluates the derivative of the following coupled ODE system at the point
#' (t, x, y):
#' 
#' dx/dt = x - y, dy/dt = x^2 + y^2 - 2.
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
#' example12.flowField <- flowField(example12, xlim = c(-4, 4), ylim = c(-4, 4),
#'                                  points = 17, add = FALSE)
#' y0                   <- matrix(c(2, 2, -3, 0, 0, 2, 0, -3), ncol = 2, nrow = 4,
#'                                byrow = TRUE)
#' example12.nullclines <- nullclines(example12, xlim = c(-4, 4), ylim = c(-4, 4),
#'                                    points = 200)
#' example12.trajectory <- trajectory(example12, y0 = y0, tlim = c(0, 10))
#' example12.manifolds  <- drawManifolds(example12, y0 = c(-1, -1), tend = 1000,
#'                                       col = c("green", "red"), add.legend = TRUE)
#' 
#' # Determine the stability of the equilibrium points.
#' example12.stability.1 <- stability(example12, ystar = c(1, 1))
#' example12.stability.2 <- stability(example12, ystar = c(-1, -1))
#' 
example12 <- function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  dy    <- numeric(2)
  dy[1] <- x - y
  dy[2] <- x^2 + y^2 - 2
  list(dy)
}
