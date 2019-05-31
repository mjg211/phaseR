#' The Lotka-Volterra model
#' 
#' The derivative function of the Lotka-Volterra model, an example of a
#' two-dimensional autonomous ODE system.
#' 
#' \code{lotkaVolterra} evaluates the derivative of the following ODE at the
#' point \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dx</i>/<i>dt</i> = <i>&lambda;x</i> -
#' <i>&epsilon;xy</i>, <i>dy</i>/<i>dt</i> = <i>&eta;xy</i> -
#' <i>&delta;y</i>.</center>}}{\deqn{\frac{dx}{dt} = \lambda x - \epsilon xy,
#' \frac{dy}{dt} = \eta xy - \delta y.}}
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
#' \code{\link[base]{numeric}} \code{\link[base]{vector}} with parameters
#' specified in the following order:
#' \ifelse{html}{\out{<i>&lambda;</i>}}{\eqn{\lambda}},
#' \ifelse{html}{\out{<i>&epsilon;</i>}}{\eqn{\epsilon}},
#' \ifelse{html}{\out{<i>&eta;</i>}}{\eqn{\eta}},
#' \ifelse{html}{\out{<i>&delta;</i>}}{\eqn{\delta}}.
#' @return Returns a \code{\link[base]{list}} containing the values of the two
#' derivatives at
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}.
#' @author Michael J Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @export
lotkaVolterra <- function(t, y, parameters) {
  list(c(parameters[1]*y[1] - parameters[2]*y[1]*y[2],
         parameters[3]*y[1]*y[2] - parameters[4]*y[2]))
}
