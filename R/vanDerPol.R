#' The Van Der Pol Oscillator
#' 
#' The derivative function of the Van Der Pol Oscillator, an example of a two
#' dimensional autonomous ODE system.
#' 
#' Evaluates the derivative of the following coupled ODE system at the point
#' (t, x, y):
#' 
#' dx/dt = y, dy/dt = mu*(1 - x^2)*y - x.
#' 
#' Format is designed to be compatible with ode from the deSolve package.
#' 
#' @param t Value of t, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y Values of x and y, the dependent variables, to evaluate the
#' derivative at. Should be a vector of length 2.
#' @param parameters Values of the parameters of the system. Should be a number
#' for the value of mu.
#' @return Returns a list dy containing the values of the two derivatives at
#' (t, x, y).
#' @author Michael J. Grayling
#' @seealso \code{\link{ode}}
#' @export
#' @examples
#' # Plot the velocity field, nullclines and several trajectories.
#' vanDerPol.flowField  <- flowField(vanDerPol, xlim = c(-5, 5), ylim = c(-5, 5),
#'                                   parameters = 3, points = 15, add = FALSE)
#' y0                   <- matrix(c(2, 0, 0, 2, 0.5, 0.5), ncol = 2, nrow = 3, byrow = TRUE)
#' vanDerPol.nullclines <- nullclines(vanDerPol, xlim = c(-5, 5), ylim = c(-5, 5),
#'                                    parameters = 3, points = 500)
#' vanDerPol.trajectory <- trajectory(vanDerPol, y0 = y0, tlim = c(0, 10), parameters = 3)
#' 
#' # Plot x and y against t.
#' vanDerPol.numericalSolution <- numericalSolution(vanDerPol, y0 = c(4, 2), tlim = c(0, 100),
#'                                                  parameters = 3)
#' 
#' # Determine the stability of the equilibrium point.
#' vanDerPol.stability <- stability(vanDerPol, ystar = c(0, 0), parameters = 3)
#' 
vanDerPol <- function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  mu <- parameters[1]
  dy    <- numeric(2)
  dy[1] <- y
  dy[2] <- mu*(1 - x^2)*y - x
  list(dy)
}
