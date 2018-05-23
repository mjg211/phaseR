#' Example ODE System 14
#' 
#' The derivative function of an example two-dimensional autonomous ODE system.
#' 
#' \code{example14} evaluates the derivatives of the following coupled ODE system
#' at the point \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{
#' \eqn{(t, x, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dx</i>/<i>dt</i> = <i>x</i><sup>2</sup> - <i>y</i> - 10,
#' <i>dy</i>/<i>dt</i> = -3<i>x</i><sup>2</sup> + <i>x</i><i>y</i>.</center>}}{\deqn{\frac{dx}{dt} = x^2 - y - 10, \frac{dy}{dt} = -3x^2 + xy.}}
#' 
#' Its format is designed to be compatible with \code{\link[deSolve]{ode}} from
#' the \code{\link[deSolve]{deSolve}} package.
#' 
#' @param t The value of \ifelse{html}{\out{<i>t</i>}}{\eqn{t}}, the independent
#' variable, to evaluate the derivative at. Should be a single number.
#' @param y The values of \ifelse{html}{\out{<i>x</i>}}{\eqn{x}} and 
#' \ifelse{html}{\out{<i>y</i>}}{\eqn{y}}, the dependent
#' variables, to evaluate the derivative at. Should be a vector of length two.
#' @param parameters The values of the parameters of the system. Not required
#' here.
#' @return Returns a list containing the values of the two derivatives
#' at \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}.
#' @author Michael J. Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @export
example14 <- function(t, y, parameters) {
  list(c(y[1]^2 - y[2] - 10, -3*y[1]^2 + y[1]*y[2]))
}
