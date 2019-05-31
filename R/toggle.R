#' The genetic toggle switch model
#' 
#' The derivative function of a simple genetic toggle switch model, an example
#' of a two-dimensional autonomous ODE system.
#' 
#' \code{toggle} evaluates the derivative of the following ODE at the point
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dx</i>/<i>dt</i> = -<i>x</i> +
#' <i>&alpha;</i>/(1 + <i>y</i><sup><i>&beta;</i></sup>),
#' <i>dy</i>/<i>dt</i> = -<i>y</i> + <i>&alpha;</i>/(1 +
#' <i>x</i><sup><i>&gamma;</i></sup>).</center>}}{\deqn{\frac{dx}{dt} =
#' -x + \alpha(1 + y^\beta), \frac{dy}{dt} = -y + \alpha(1 + x^\gamma).}}
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
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}},
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}},
#' \ifelse{html}{\out{<i>&gamma;</i>}}{\eqn{\gamma}}.
#' @return Returns a \code{\link[base]{list}} containing the values of the two
#' derivatives at
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}.
#' @author Michael J Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @export
toggle <- function(t, y, parameters) {
  list(c(-y[1] + parameters[1]/(1 + y[2]^parameters[2]),
         -y[2] + parameters[1]/(1 + y[1]^parameters[3]))) 
}
