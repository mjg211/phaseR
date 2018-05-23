#' The Logistic Growth Model
#' 
#' The derivative function of the logistic growth model, an example of a two-dimensional autonomous ODE system.
#' 
#' \code{logistic} evaluates the derivative of the following ODE at the point
#' \ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dy</i>/<i>dt</i> = <i>&beta;y</i>(1 - <i>y</i>/<i>K</i>).</center>}}{
#' \deqn{\frac{dy}{dt} = \beta y(1 - y/K).}}
#' 
#' Its format is designed to be compatible with \code{\link[deSolve]{ode}} from the \code{\link[deSolve]{deSolve}} package.
#' 
#' @param t The value of \ifelse{html}{\out{<i>t</i>}}{\eqn{t}}, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y The value of \ifelse{html}{\out{<i>y</i>}}{\eqn{y}}, the dependent variable, to evaluate the derivative at.
#' Should be a single number.
#' @param parameters The values of the parameters of the system. Should be a vector
#' with parameters specified in the following order: \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}, \ifelse{html}{\out{<i>K</i>}}{\eqn{K}}.
#' @return Returns a list containing the value of the derivative at \ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}.
#' @author Michael J. Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @examples
#' # Plot the velocity field, nullclines and several trajectories
#' logistic.flowField     <- flowField(logistic,
#'                                     xlim = c(0, 5),
#'                                     ylim = c(-1, 3),
#'                                     parameters = c(1, 2),
#'                                     points = 21,
#'                                     system = "one.dim",
#'                                     add = FALSE)
#' logistic.nullclines    <- nullclines(logistic,
#'                                      xlim = c(0, 5),
#'                                      ylim = c(-1, 3),
#'                                      parameters = c(1, 2),
#'                                      system = "one.dim")
#' logistic.trajectory    <- trajectory(logistic,
#'                                      y0 = c(-0.5, 0.5, 1.5, 2.5),
#'                                      tlim = c(0, 5),
#'                                      parameters = c(1, 2),
#'                                      system = "one.dim")
#' # Plot the phase portrait
#' logistic.phasePortrait <- phasePortrait(logistic,
#'                                         ylim = c(-0.5, 2.5),
#'                                         parameters = c(1, 2),
#'                                         points = 10,
#'                                         frac = 0.5)
#' # Determine the stability of the equilibrium points
#' logistic.stability.1   <- stability(logistic,
#'                                     ystar = 0,
#'                                     parameters = c(1, 2),
#'                                     system = "one.dim")
#' logistic.stability.2   <- stability(logistic,
#'                                     ystar = 2,
#'                                     parameters = c(1, 2),
#'                                     system = "one.dim")
#' @export
logistic <- function(t, y, parameters) {
  list(parameters[1]*y*(1 - y/parameters[2]))
}
