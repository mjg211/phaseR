#' Example ODE system 12
#' 
#' The derivative function of an example two-dimensional autonomous ODE system.
#' 
#' \code{example12} evaluates the derivatives of the following coupled ODE
#' system at the point \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{
#' \eqn{(t, x, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dx</i>/<i>dt</i> = <i>x</i> - <i>y</i>,
#' <i>dy</i>/<i>dt</i> = <i>x</i><sup>2</sup> + <i>y</i><sup>2</sup> -
#' 2.</center>}}{\deqn{\frac{dx}{dt} = x - y, \frac{dy}{dt} = x^2 + y^2 - 2.}}
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
#' example12_flowField   <- flowField(example12,
#'                                    xlim   = c(-4, 4),
#'                                    ylim   = c(-4, 4),
#'                                    points = 17,
#'                                    add    = FALSE)
#' y0                    <- matrix(c(2, 2, -3, 0,
#'                                   0, 2, 0, -3), 4, 2,
#'                                 byrow = TRUE)
#' example12_nullclines  <- nullclines(example12,
#'                                     xlim   = c(-4, 4),
#'                                     ylim   = c(-4, 4),
#'                                     points = 200)
#' example12_trajectory  <- trajectory(example12,
#'                                     y0   = y0,
#'                                     tlim = c(0, 10))
#' example12_manifolds   <- drawManifolds(example12,
#'                                        y0         = c(-1, -1),
#'                                        tend       = 1000,
#'                                        col        = c("green", "red"),
#'                                        add.legend = TRUE)
#' # Determine the stability of the equilibrium points
#' example12_stability_1 <- stability(example12,
#'                                    ystar = c(1, 1))
#' example12_stability_2 <- stability(example12,
#'                                    ystar = c(-1, -1))
#' @export
example12 <- function(t, y, parameters) {
  list(c(y[1] - y[2], y[1]^2 + y[2]^2 - 2))
}
