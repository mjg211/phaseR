#' Genetic Toggle Switch Model
#' 
#' The derivative function of a simple genetic toggle switch model, an example
#' of a two dimensional autonomous ODE system.
#' 
#' Evaluates the derivative of the following coupled ODE system at the point
#' (t, x, y):
#' 
#' dx/dt = -x + alpha/(1 + y^beta), dy/dt = -y + alpha/(1 + x^gamma).
#' 
#' Format is designed to be compatible with ode from the deSolve package.
#' 
#' @param t Value of t, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y Values of x and y, the dependent variables, to evaluate the
#' derivative at. Should be a vector of length 2.
#' @param parameters Values of the parameters of the system. Should be a vector
#' with parameters specified in the following order: alpha, beta, gamma.
#' @return Returns a list dy containing the values of the two derivatives at
#' (t, x, y).
#' @author Michael J. Grayling, Stephen P. Ellner, John M. Guckenheimer
#' @export
#' @seealso \code{\link{ode}}
toggle <- function(t, y, parameters){
  x  <- y[1]
  y  <- y[2]
  alpha <- parameters[1]
  beta  <- parameters[2]
  gamma <- parameters[3]
  dy    <- numeric(2)
  dy[1] <- -x + alpha/(1 + y^beta)
  dy[2] <- -y + alpha/(1 + x^gamma)
  list(dy) 
}
