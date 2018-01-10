#' Morris-Lecar Model
#' 
#' The derivative function of the Morris-Lecar model, an example of a two
#' dimensional autonomous ODE system.
#' 
#' Evaluates the derivative of the following coupled ODE system at the point
#' (t, x, y):
#' 
#' dx/dt = (1/20)*(90 - gca*0.5*(1 + tanh((x + 1.2)/18))*(x - 120) - 8*y*(x +
#' 84) - 2*(x + 60)), dy/dt = phi*(0.5*(1 + tanh((x - 2)/30)) - y)*cosh((x -
#' 2)/60).
#' 
#' Format is designed to be compatible with ode from the deSolve package.
#' 
#' @param t Value of t, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y Values of x and y, the dependent variables, to evaluate the
#' derivative at. Should be a vector of length 2.
#' @param parameters Values of the parameters of the system. Should be a vector
#' with parameters specified in the following order: gca, phi.
#' @return Returns a list dy containing the values of the two derivatives at
#' (t, x, y).
#' @author Michael J. Grayling, Stephen P. Ellner, John M. Guckenheimer
#' @export
#' @seealso \code{\link{ode}}
morrisLecar <- function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  gca <- parameters[1]
  phi <- parameters[2]
  dy    <- numeric(2)
  dy[1] <- (1/20)*(90 - gca*0.5*(1 + tanh((x + 1.2)/18))*(x - 120) -
                     8*y*(x + 84) - 2*(x + 60))
  dy[2] <- phi*(0.5*(1 + tanh((x - 2)/30)) - y)*cosh((x - 2)/60)
  list(dy)
}
