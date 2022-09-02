#' Example ODE system 11
#'
#' The derivative function of an example two-dimensional autonomous ODE system.
#'
#' \code{example11} evaluates the derivatives of the following coupled ODE
#' system at the point \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{
#' \eqn{(t, x, y)}}:
#'
#' \ifelse{html}{\out{<i>dx</i>/<i>dt</i> = <i>x</i>(3 - <i>x</i> -
#' 2<i>y</i>), <i>dy</i>/<i>dt</i> = -<i>y</i>(2 - <i>x</i> -
#' <i>y</i>).}}{\deqn{\frac{dx}{dt} = x(3 - x - 2y), \frac{dy}{dt} =
#' -y(2 - x - y).}}
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
#' example11_flowField   <- flowField(example11,
#'                                    xlim   = c(-5, 5),
#'                                    ylim   = c(-5, 5),
#'                                    points = 21,
#'                                    add    = FALSE)
#' y0                    <- matrix(c(4, 4, -1, -1,
#'                                   -2, 1, 1, -1), 4, 2,
#'                                 byrow = TRUE)
#' example11_nullclines  <- nullclines(example11,
#'                                     xlim   = c(-5, 5),
#'                                     ylim   = c(-5, 5),
#'                                     points = 200)
#' example11_trajectory  <- trajectory(example11,
#'                                     y0   = y0,
#'                                     tlim = c(0, 10))
#' # Determine the stability of the equilibrium points
#' example11_stability_1 <- stability(example11, ystar = c(0, 0))
#' example11_stability_2 <- stability(example11, ystar = c(0, 2))
#' example11_stability_3 <- stability(example11, ystar = c(1, 1))
#' example11_stability_4 <- stability(example11, ystar = c(3, 0))
#' @export
example11 <- function(t, y, parameters) {
  list(c(y[1]*(3 - y[1] - 2*y[2]), y[2]*(2 - y[1] - y[2])))
}
