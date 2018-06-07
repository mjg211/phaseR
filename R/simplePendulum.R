#' The Simple Pendulum Model
#' 
#' The derivative function of the simple pendulum model, an example of a two-dimensional autonomous ODE system.
#' 
#' \code{simplePendulum} evaluates the derivative of the following ODE at the point
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dx</i>/<i>dt</i> = <i>y</i>,
#' <i>dy</i>/<i>dt</i> = -<i>g</i> sin(<i>x</i>)/<i>l</i>.</center>}}{\deqn{\frac{dx}{dt} = y, \frac{dy}{dt} = \frac{-g\sin(x)}{l}.}}
#' 
#' Its format is designed to be compatible with \code{\link[deSolve]{ode}} from the \code{\link[deSolve]{deSolve}} package.
#' 
#' @param t The value of \ifelse{html}{\out{<i>t</i>}}{\eqn{t}}, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y The values of \ifelse{html}{\out{<i>x</i>}}{\eqn{x}} and 
#' \ifelse{html}{\out{<i>y</i>}}{\eqn{y}}, the dependent
#' variables, to evaluate the derivative at. Should be a vector of length two.
#' @param parameters The values of the parameters of the system. Should be a single number prescribing the value of \ifelse{html}{\out{<i>l</i>}}{\eqn{l}}.
#' @return Returns a list containing the values of the two derivatives
#' at \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}.
#' @author Michael J. Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @examples
#' # Plot the velocity field, nullclines and several trajectories
#' simplePendulum.flowField   <- flowField(simplePendulum,
#'                                         xlim = c(-7, 7),
#'                                         ylim = c(-7, 7),
#'                                         parameters = 5,
#'                                         points = 19,
#'                                         add = FALSE)
#' y0                         <- matrix(c(0, 1, 0, 4, -6, 1, 5, 0.5, 0, -3),
#'                                      5, 2, byrow = TRUE)
#' simplePendulum.nullclines  <- nullclines(simplePendulum,
#'                                          xlim = c(-7, 7),
#'                                          ylim = c(-7, 7),
#'                                          parameters = 5,
#'                                          points = 500)
#' simplePendulum.trajectory  <- trajectory(simplePendulum,
#'                                          y0 = y0,
#'                                          tlim = c(0, 10),
#'                                          parameters = 5)
#' simplePendulum.manifolds   <- drawManifolds(simplePendulum,
#'                                             y0 = c(pi, 0),
#'                                             parameters = 5,
#'                                             tend = 1000,
#'                                             col = c("green", "red"),
#'                                             add.legend = TRUE)
#' # Determine the stability of two equilibrium points
#' simplePendulum.stability.1 <- stability(simplePendulum,
#'                                         ystar = c(0, 0),
#'                                         parameters = 5)
#' simplePendulum.stability.2 <- stability(simplePendulum,
#'                                         ystar = c(pi, 0),
#'                                         parameters = 5)
#' @export
simplePendulum <- function(t, y, parameters){
  list(c(y[2], -9.81*sin(y[1])/parameters))
}
