#' The Lindemann Mechanism
#' 
#' The derivative function of the non-dimensional version of the Lindemann
#' Mechanism, an example of a two dimensional autonomous ODE system.
#' 
#' Evaluates the derivative of the following coupled ODE system at the point
#' (t, x, y):
#' 
#' dx/dt = -x^2 + alpha*x*y, dy/dt = x^2 - alpha*x*y - y.
#' 
#' Format is designed to be compatible with ode from the deSolve package.
#' 
#' @param t Value of t, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y Values of x and y, the dependent variables, to evaluate the
#' derivative at. Should be a vector of length 2.
#' @param parameters Values of the parameters of the system. Should be a number
#' for the value of alpha.
#' @return Returns a list dy containing the values of the two derivatives at
#' (t, x, y).
#' @author Michael J. Grayling
#' @export
#' @seealso \code{\link{ode}}
lindemannMechanism <- function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  alpha <- parameters[1]
  dy    <- numeric(2)
  dy[1] <- -x^2 + alpha*x*y
  dy[2] <- x^2 - alpha*x*y - y
  list(dy)
}
