#' The Lindemann mechanism
#' 
#' The derivative function of the non-dimensional version of the Lindemann
#' mechanism, an example of a two-dimensional autonomous ODE system.
#' 
#' \code{lindemannMechanism} evaluates the derivative of the following ODE at
#' the point \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{
#' \eqn{(t, x, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dx</i>/<i>dt</i> = -<i>x</i><sup>2</sup> +
#' <i>&alpha;xy</i>, <i>dy</i>/<i>dt</i> = <i>x</i><sup>2</sup> -
#' <i>&alpha;xy</i> - <i>y</i>.</center>}}{\deqn{\frac{dx}{dt} = -x^2 +
#' \alpha xy, \frac{dy}{dt} = x^2 - \alpha xy - y.}}
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
#' @param parameters The values of the parameters of the system. Should be a
#' \code{\link[base]{numeric}} \code{\link[base]{vector}} prescribing the value
#' of \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}.
#' @return Returns a \code{\link[base]{list}} containing the values of the two
#' derivatives at
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}.
#' @author Michael J Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @export
lindemannMechanism <- function(t, y, parameters) {
  list(c(-y[1]^2 + parameters*y[1]*y[2], y[1]^2 - parameters*y[1]*y[2] - y[2]))
}
