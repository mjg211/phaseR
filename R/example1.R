#' Example ODE System 1
#' 
#' The derivative function of an example one-dimensional autonomous ODE system.
#' 
#' \code{example1} evaluates the derivative of the following ODE at the point \ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dy</i>/<i>dt</i> = 4 - <i>y</i><sup>2</sup>.</center>}}{\deqn{\frac{dy}{dt} = 4 - y^2.}}
#' 
#' Its format is designed to be compatible with \code{\link[deSolve]{ode}} from the \code{\link[deSolve]{deSolve}} package.
#' 
#' @param t The value of \ifelse{html}{\out{<i>t</i>}}{\eqn{t}}, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y The value of \ifelse{html}{\out{<i>y</i>}}{\eqn{y}}, the dependent variable, to evaluate the derivative at.
#' Should be a single number.
#' @param parameters The values of the parameters of the system. Not required here.
#' @return Returns a list containing the value of the derivative at \ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}.
#' @author Michael J. Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @export
example1 <- function(t, y, parameters) {
  list(4 - y^2)
}
