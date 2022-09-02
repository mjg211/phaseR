#' The Van der Pol oscillator
#'
#' The derivative function of the Van der Pol Oscillator, an example of a
#' two-dimensional autonomous ODE system.
#'
#' \code{vanDerPol} evaluates the derivative of the following ODE at the point
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}:
#'
#' \ifelse{html}{\out{<i>dx</i>/<i>dt</i> = <i>y</i>,
#' <i>dy</i>/<i>dt</i> = <i>&mu;</i>(1 - <i>x</i><sup>2</sup>)<i>y</i> -
#' <i>x</i>.}}{\deqn{\frac{dx}{dt} = y, \frac{dy}{dt}= \mu(1 - x^2)y - x.}}
#'
#' Its format is designed to be compatible with \code{\link[deSolve]{ode}} from
#' the \code{\link[deSolve]{deSolve}} package.
#'
#' @param t The value of \ifelse{html}{\out{<i>t</i>}}{\eqn{t}}, the independent
#' variable, to evaluate the derivative at. Should be a
#' \code{\link[base]{numeric}} \code{\link[base]{vector}} of
#' \code{\link[base]{length}} one.
#' @param y The values of \ifelse{html}{\out{<i>x</i>}}{\eqn{x}} and
#' \ifelse{html}{\out{<i>y</i>}}{\eqn{y}}, the dependent variables, to evaluate
#' the derivative at. Should be a \code{\link[base]{numeric}}
#' \code{\link[base]{vector}} of \code{\link[base]{length}} two.
#' @param parameters The values of the parameters of the system. Should be a
#' \code{\link[base]{numeric}} \code{\link[base]{vector}} prescribing the value
#' of \ifelse{html}{\out{<i>&mu;</i>}}{\eqn{\mu}}.
#' @return Returns a \code{\link[base]{list}} containing the values of the two
#' derivatives at
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}.
#' @author Michael J Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @examples
#' # Plot the velocity field, nullclines and several trajectories.
#' vanDerPol_flowField         <- flowField(vanDerPol,
#'                                          xlim       = c(-5, 5),
#'                                          ylim       = c(-5, 5),
#'                                          parameters = 3,
#'                                          points     = 15,
#'                                          add        = FALSE)
#' y0                          <- matrix(c(2, 0, 0, 2, 0.5, 0.5), 3, 2,
#'                                       byrow = TRUE)
#' \donttest{
#' vanDerPol_nullclines        <- nullclines(vanDerPol,
#'                                           xlim       = c(-5, 5),
#'                                           ylim       = c(-5, 5),
#'                                           parameters = 3,
#'                                           points     = 500)
#' }
#' vanDerPol_trajectory        <- trajectory(vanDerPol,
#'                                           y0         = y0,
#'                                           tlim       = c(0, 10),
#'                                           parameters = 3)
#' # Plot x and y against t
#' vanDerPol_numericalSolution <- numericalSolution(vanDerPol,
#'                                                  y0         = c(4, 2),
#'                                                  tlim       = c(0, 100),
#'                                                  parameters = 3)
#' # Determine the stability of the equilibrium point
#' vanDerPol_stability         <- stability(vanDerPol,
#'                                          ystar      = c(0, 0),
#'                                          parameters = 3)
#' @export
vanDerPol <- function(t, y, parameters) {
  list(c(y[2], parameters*(1 - y[1]^2)*y[2] - y[1]))
}
