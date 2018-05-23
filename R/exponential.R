#' The Exponential Growth Model
#' 
#' The derivative function of the exponential growth model, an example of a one-
#' dimensional autonomous ODE system.
#' 
#' \code{exponential} evaluates the derivative of the following ODE at the point
#' \ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dy</i>/<i>dt</i> = <i>&beta;y</i>.</center>}}{
#' \deqn{\frac{dy}{dt} = \beta y.}}
#' 
#' Its format is designed to be compatible with \code{\link[deSolve]{ode}} from the \code{\link[deSolve]{deSolve}} package.
#' 
#' @param t The value of \ifelse{html}{\out{<i>t</i>}}{\eqn{t}}, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y The value of \ifelse{html}{\out{<i>y</i>}}{\eqn{y}}, the dependent variable, to evaluate the derivative at.
#' Should be a single number.
#' @param parameters The values of the parameters of the system. Should be a single number prescribing the value of \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}.
#' @return Returns a list containing the value of the derivative at \ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}.
#' @author Michael J. Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @export
exponential <- function(t, y, parameters) {
  list(parameters*y)
}
