#' Example ODE System Number Five
#' 
#' The derivative function of an example two dimensional autonomous ODE system.
#' 
#' Evaluates the derivatives of the following coupled ODE system at the point
#' (t,x,y):
#' 
#' dx/dt = 2*x + y, dy/dt = 2*x - y.
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
#' 
#' # Plot the velocity field, nullclines, manifolds and several trajectories.
#' example5.flowField  <- flowField(example5, xlim = c(-3, 3), ylim = c(-3, 3),
#'                                  points = 19, add = FALSE)
#' y0                  <- matrix(c(1, 0, -1, 0, 2, 2, -2, 2, 0, 3, 0, -3), ncol = 2,
#'                               nrow = 6, byrow = TRUE)
#' example5.nullclines <- nullclines(example5, xlim = c(-3, 3), ylim = c(-3, 3))
#' example5.trajectory <- trajectory(example5, y0 = y0, tlim = c(0,10))
#' example5.manifolds <- drawManifolds(example5, y0 = c(0, 0), tend = 1000,
#'                                     col = c("green", "red"), add.legend = TRUE)
#' 
#' # Plot x and y against t.
#' example5.numericalSolution <- numericalSolution(example5, y0 = c(0, 3), tlim = c(0, 3))
#' 
#' # Determine the stability of the equilibrium point.
#' example5.stability <- stability(example5, ystar = c(0, 0))
#' 
example5 <- function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  dy    <- numeric(2)
  dy[1] <- 2*x + y
  dy[2] <- 2*x - y
  list(dy)
}
