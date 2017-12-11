#' Example ODE System Number One
#' 
#' The derivative function of an example one dimensional autonomus ODE system.
#' 
#' Evaluates the derivative of the following ODE at the point (t, y):
#' 
#' dy/dt = 4 - y^2.
#' 
#' Format is designed to be compatible with ode from the deSolve package.
#' 
#' @param t Value of t, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y Value of y, the dependent variable, to evaluate the derivative at.
#' Should be a single number.
#' @param parameters Values of the parameters of the system. Not required here.
#' @return Returns a list dy containing the value of the derivative at (t, y).
#' @author Michael J. Grayling
#' @seealso \code{\link{ode}}
#' @export
example1 <- function(t, y, parameters){
  dy <- 4 - y^2
  list(dy)
}
