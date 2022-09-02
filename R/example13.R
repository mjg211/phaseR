#' Example ODE system 13
#'
#' The derivative function of an example two-dimensional autonomous ODE system.
#'
#' \code{example13} evaluates the derivatives of the following coupled ODE
#' system at the point \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{
#' \eqn{(t, x, y)}}:
#'
#' \ifelse{html}{\out{<i>dx</i>/<i>dt</i> = 2 - <i>x</i><sup>2</sup> -
#' <i>y</i><sup>2</sup>, <i>dy</i>/<i>dt</i> = <i>x</i><sup>2</sup> -
#' <i>y</i><sup>2</sup>.}}{\deqn{\frac{dx}{dt} = 2 - x^2 - y^2,
#' \frac{dy}{dt} = x^2 - y^2.}}
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
#' @export
example13 <- function(t, y, parameters) {
  list(c(2 - y[1]^2 - y[2]^2, y[1]^2 - y[2]^2))
}
