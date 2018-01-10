#' The Exponential Growth Model
#' 
#' The derivative function of the exponential growth model, an example of a one
#' dimensional autonomous ODE system.
#' 
#' Evaluates the derivative of the following ODE at the point (t, y):
#' 
#' dy/dt = beta*y.
#' 
#' Format is designed to be compatible with ode from the deSolve package.
#' 
#' @param t Value of t, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y Value of y, the dependent variable, to evaluate the derivative at.
#' Should be a single number.
#' @param parameters Values of the parameters of the system. Should be a number
#' for the value of beta.
#' @return Returns a list dy containing the value of the derivative at (t, y).
#' @author Michael J. Grayling
#' @export
#' @seealso \code{\link{ode}}
exponential <- function(t, y, parameters){
  beta <- parameters[1]
  dy <- beta*y
  list(dy)
}
