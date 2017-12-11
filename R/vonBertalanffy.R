#' The von Bertalanffy Growth Model
#' 
#' The derivative function of the von Bertalanffy growth model, an example of a
#' one dimensional autonomous ODE system.
#' 
#' Evaluates the derivative of the following ODE at the point (t, y):
#' 
#' dy/dt = alpha*y^(2/3) - beta*y.
#' 
#' Format is designed to be compatible with ode from the deSolve package.
#' 
#' @param t Value of t, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y Value of y, the dependent variable, to evaluate the derivative at.
#' Should be a single number.
#' @param parameters Values of the parameters of the system. Should be a vector
#' with parameters specified in the following order: alpha, beta.
#' @return Returns a list dy containing the value of the derivative at (t, y).
#' @author Michael J. Grayling
#' @export
#' @seealso \code{\link{ode}}
vonBertalanffy <- function(t, y, parameters){
  alpha <- parameters[1]
  beta  <- parameters[2]
  if (y >= 0) {
    dy <- alpha*(y^(2/3)) - beta*y
  }
  if (y < 0) {
    dy <- alpha*(-abs(y)^(2/3)) - beta*y
  }
  list(dy)
}
