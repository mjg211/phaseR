#' Example ODE System 3
#' 
#' The derivative function of an example two-dimensional autonomous ODE system.
#' 
#' \code{example3} evaluates the derivatives of the following coupled ODE system
#' at the point \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{
#' \eqn{(t, x, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dx</i>/<i>dt</i> = -<i>x</i>,
#' <i>dy</i>/<i>dt</i> = -4<i>x</i>.</center>}}{\deqn{\frac{dx}{dt} = -x, \frac{dy}{dt} = -4x.}}
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
example3 <- function(t, y, parameters) {
  list(c(-y[1], -4*y[1]))
}