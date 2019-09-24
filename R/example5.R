#' Example ODE system 5
#'
#' The derivative function of an example two-dimensional autonomous ODE system.
#'
#' \code{example5} evaluates the derivatives of the following coupled ODE system
#' at the point \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{
#' \eqn{(t, x, y)}}:
#'
#' \ifelse{html}{\out{<center><i>dx</i>/<i>dt</i> = 2<i>x</i> + <i>y</i>,
#' <i>dy</i>/<i>dt</i> = 2<i>x</i> -
#' <i>y</i>.</center>}}{\deqn{\frac{dx}{dt} = 2x + y, \frac{dy}{dt} = 2x - y.}}
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
#' @param parameters The values of the parameters of the system. Not used here.
#' @return Returns a \code{\link[base]{list}} containing the values of the two
#' derivatives at
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}.
#' @author Michael J Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @examples
#' # Plot the velocity field, nullclines, manifolds and several trajectories
#' example5_flowField         <- flowField(example5,
#'                                         xlim   = c(-3, 3),
#'                                         ylim   = c(-3, 3),
#'                                         points = 19,
#'                                         add    = FALSE)
#' y0                         <- matrix(c(1, 0, -1, 0, 2, 2,
#'                                        -2, 2, 0, 3, 0, -3), 6, 2,
#'                                      byrow = TRUE)
#' example5_nullclines        <- nullclines(example5,
#'                                          xlim = c(-3, 3),
#'                                          ylim = c(-3, 3))
#' example5_trajectory        <- trajectory(example5,
#'                                          y0   = y0,
#'                                          tlim = c(0,10))
#' # Plot x and y against t
#' example5_numericalSolution <- numericalSolution(example5,
#'                                                 y0   = c(0, 3),
#'                                                 tlim = c(0, 3))
#' # Determine the stability of the equilibrium point
#' example5_stability         <- stability(example5,
#'                                         ystar = c(0, 0))
#' @export
example5 <- function(t, y, parameters) {
  list(c(2*y[1] + y[2], 2*y[1] - y[2]))
}
