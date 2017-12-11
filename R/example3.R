#' Example ODE System Number Three
#' 
#' The derivative function of an example two dimensional autonomous ODE system.
#' 
#' Evaluates the derivatives of the following coupled ODE system at the point
#' (t, x, y):
#' 
#' dx/dt = -x, dy/dt = -4*x.
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
example3 <- function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  dy    <- numeric(2)
  dy[1] <- -x
  dy[2] <- -4*x
  list(dy)
}
