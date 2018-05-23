#' The Van Der Pol Oscillator
#' 
#' The derivative function of the Van Der Pol Oscillator, an example of a two-dimensional autonomous ODE system.
#' 
#' \code{vanDerPol} evaluates the derivative of the following ODE at the point
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dx</i>/<i>dt</i> = <i>y</i>,
#' <i>dy</i>/<i>dt</i> = <i>&mu;</i>(1 - <i>x</i><sup>2</sup>)<i>y</i> - <i>x</i>.</center>}}{\deqn{\frac{dx}{dt} = y, \frac{dy}{dt}= \mu(1 - x^2)y - x.}}
#' 
#' Its format is designed to be compatible with \code{\link[deSolve]{ode}} from the \code{\link[deSolve]{deSolve}} package.
#' 
#' @param t The value of \ifelse{html}{\out{<i>t</i>}}{\eqn{t}}, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y The values of \ifelse{html}{\out{<i>x</i>}}{\eqn{x}} and 
#' \ifelse{html}{\out{<i>y</i>}}{\eqn{y}}, the dependent
#' variables, to evaluate the derivative at. Should be a vector of length two.
#' @param parameters The values of the parameters of the system. Should be a single number prescribing the value of \ifelse{html}{\out{<i>&mu;</i>}}{\eqn{\mu}}.
#' @return Returns a list containing the values of the two derivatives
#' at \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}.
#' @author Michael J. Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @examples
#' # Plot the velocity field, nullclines and several trajectories.
#' vanDerPol.flowField         <- flowField(vanDerPol,
#'                                          xlim = c(-5, 5),
#'                                          ylim = c(-5, 5),
#'                                          parameters = 3,
#'                                          points = 15,
#'                                          add = FALSE)
#' y0                          <- matrix(c(2, 0, 0, 2, 0.5, 0.5), 3, 2,
#'                                       byrow = TRUE)
#' vanDerPol.nullclines        <- nullclines(vanDerPol,
#'                                           xlim = c(-5, 5),
#'                                           ylim = c(-5, 5),
#'                                           parameters = 3,
#'                                           points = 500)
#' vanDerPol.trajectory        <- trajectory(vanDerPol,
#'                                           y0 = y0,
#'                                           tlim = c(0, 10),
#'                                           parameters = 3)
#' # Plot x and y against t
#' vanDerPol.numericalSolution <- numericalSolution(vanDerPol,
#'                                                  y0 = c(4, 2),
#'                                                  tlim = c(0, 100),
#'                                                  parameters = 3)
#' # Determine the stability of the equilibrium point
#' vanDerPol.stability         <- stability(vanDerPol,
#'                                          ystar = c(0, 0),
#'                                          parameters = 3)
#' @export
vanDerPol <- function(t, y, parameters) {
  list(c(y[2], parameters*(1 - y[1]^2)*y[2] - y[1]))
}
