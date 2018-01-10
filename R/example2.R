#' Example ODE System Number Two
#' 
#' The derivative function of an example one dimensional autonomous ODE system.
#' 
#' Evaluates the derivative of the following ODE at the point (t, y):
#' 
#' dy/dt = y*(1 - y)*(2 - y).
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
#' @examples
#' # Plot the flow field and several trajectories.
#' example2.flowField  <- flowField(example2, xlim = c(0, 4), ylim = c(-1, 3),
#'                                  points = 21, system = "one.dim", add = FALSE,
#'                                  xlab = "t")
#' example2.trajectory <- trajectory(example2, y0 = c(-0.5, 0.5, 1.5, 2.5), tlim = c(0, 4),
#'                                   system = "one.dim")
#' 
#' # Plot the phase portrait.
#' example2.phasePortrait <- phasePortrait(example2, ylim = c(-0.5, 2.5), points = 10,
#'                                         frac = 0.5)
#' 
#' # Determine stability of the equilibrium points.
#' example2.stability.1 <- stability(example2, ystar = 0, system = "one.dim")
#' example2.stability.2 <- stability(example2, ystar = 1, system = "one.dim")
#' example2.stability.3 <- stability(example2, ystar = 2, system = "one.dim")
#' 
example2 <- function(t, y, parameters){
  dy <- y*(1 - y)*(2 - y)
  list(dy)
}
