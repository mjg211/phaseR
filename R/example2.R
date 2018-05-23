#' Example ODE System 2
#' 
#' The derivative function of an example one-dimensional autonomous ODE system.
#' 
#' \code{example2} evaluates the derivative of the following ODE at the point
#' \ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dy</i>/<i>dt</i> = <i>y</i>(1 - <i>y</i>)(2 - 
#' <i>y</i>).</center>}}{\deqn{dy/dt = y(1 - y)(2 - y).}}
#' 
#' Its format is designed to be compatible with \code{\link[deSolve]{ode}} from
#' the \code{\link[deSolve]{deSolve}} package.
#' 
#' @param t The value of \ifelse{html}{\out{<i>t</i>}}{\eqn{t}}, the independent
#' variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y The value of \ifelse{html}{\out{<i>y</i>}}{\eqn{y}}, the dependent
#' variable, to evaluate the derivative at.
#' Should be a single number.
#' @param parameters The values of the parameters of the system. Not required
#' here.
#' @return Returns a list containing the value of the derivative at
#' \ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}.
#' @author Michael J. Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @examples
#' # Plot the flow field and several trajectories
#' example2.flowField     <- flowField(example2,
#'                                     xlim = c(0, 4),
#'                                     ylim = c(-1, 3),
#'                                     system = "one.dim",
#'                                     add = FALSE,
#'                                     xlab = "t")
#' example2.trajectory    <- trajectory(example2,
#'                                      y0 = c(-0.5, 0.5, 1.5, 2.5),
#'                                      tlim = c(0, 4),
#'                                      system = "one.dim")
#' # Plot the phase portrait
#' example2.phasePortrait <- phasePortrait(example2,
#'                                         ylim = c(-0.5, 2.5),
#'                                         frac = 0.5)
#' # Determine the stability of the equilibrium points
#' example2.stability.1   <- stability(example2,
#'                                     ystar = 0,
#'                                     system = "one.dim")
#' example2.stability.2   <- stability(example2,
#'                                     ystar = 1,
#'                                     system = "one.dim")
#' example2.stability.3   <- stability(example2,
#'                                     ystar = 2,
#'                                     system = "one.dim")
#' @export
example2 <- function(t, y, parameters) {
  list(y*(1 - y)*(2 - y))
}