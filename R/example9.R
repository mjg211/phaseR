#' Example ODE system 9
#'
#' The derivative function of an example two-dimensional autonomous ODE system.
#'
#' \code{example9} evaluates the derivatives of the following coupled ODE system
#' at the point \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{
#' \eqn{(t, x, y)}}:
#'
#' \ifelse{html}{\out{<i>dx</i>/<i>dt</i> = -2<i>x</i> + 3<i>y</i>,
#' <i>dy</i>/<i>dt</i> = 7<i>x</i> + 6<i>y</i>.}}{\deqn{\frac{dx}{dt} =
#' -2x + 3y, \frac{dy}{dt} = 7x + 6y.}}
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
#' # Plot the velocity field, nullclines and several trajectories
#' example9_flowField  <- flowField(example9,
#'                                  xlim   = c(-3, 3),
#'                                  ylim   = c(-3, 3),
#'                                  points = 19,
#'                                  add    = FALSE)
#' y0                  <- matrix(c(1, 0, -3, 2,
#'                                 2, -2, -2, -2), 4, 2,
#'                               byrow = TRUE)
#' example9_nullclines <- nullclines(example9,
#'                                   xlim = c(-3, 3),
#'                                   ylim = c(-3, 3))
#' example9_trajectory <- trajectory(example9,
#'                                   y0   = y0,
#'                                   tlim = c(0, 10))
#' # Determine the stability of the equilibrium point
#' example9_stability  <- stability(example9,
#'                                  ystar = c(0, 0))
#' @export
example9 <- function(t, y, parameters) {
  list(c(-2*y[1] + 3*y[2], 7*y[1] + 6*y[2]))
}
