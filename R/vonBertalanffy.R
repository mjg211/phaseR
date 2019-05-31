#' The von Bertalanffy growth model
#' 
#' The derivative function of the von Bertalanffy growth model, an example of a
#' one-dimensional autonomous ODE system.
#' 
#' \code{vonBertalanffy} evaluates the derivative of the following ODE at the
#' point \ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dy</i>/<i>dt</i> =
#' <i>&alpha;y</i><sup>2/3</sup> -
#' <i>&beta;y</i>.</center>}}{\deqn{\frac{dy}{dt} = \alpha y^{2/3} - \beta y.}}
#' 
#' Its format is designed to be compatible with \code{\link[deSolve]{ode}} from
#' the \code{\link[deSolve]{deSolve}} package.
#' 
#' @param t The value of \ifelse{html}{\out{<i>t</i>}}{\eqn{t}}, the independent
#' variable, to evaluate the derivative at. Should be a
#' \code{\link[base]{numeric}} \code{\link[base]{vector}} of
#' \code{\link[base]{length}} one.
#' @param y The value of \ifelse{html}{\out{<i>y</i>}}{\eqn{y}}, the dependent
#' variable, to evaluate the derivative at. Should be a
#' \code{\link[base]{numeric}} \code{\link[base]{vector}} of
#' \code{\link[base]{length}} one.
#' @param parameters The values of the parameters of the system. Should be a
#' \code{\link[base]{numeric}} \code{\link[base]{vector}} with parameters
#' specified in the following order:
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}},
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}.
#' @return Returns a \code{\link[base]{list}} containing the values of the two
#' derivatives at
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}.
#' @author Michael J Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @export
vonBertalanffy <- function(t, y, parameters) {
  if (y >= 0) {
    list(parameters[1]*y^(2/3) - parameters[2]*y)
  } else {
    list(parameters[1]*(-abs(y)^(2/3)) - parameters[2]*y)
  }
}
