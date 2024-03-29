#' The Morris-Lecar model
#'
#' The derivative function of the Morris-Lecar model, an example of a
#' two-dimensional autonomous ODE system.
#'
#' \code{morrisLecar} evaluates the derivative of the following ODE at the point
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}:
#'
#' \ifelse{html}{\out{<i>dx</i>/<i>dt</i> = 0.05(90 -
#' 0.5<i>g</i><sub>Ca</sub>(1 + tanh(<i>x</i> + 1.2)/18))(<i>x</i> - 120) -
#' 8<i>y</i>(<i>x</i> + 84) - 2(<i>x</i> + 60),}}{\deqn{\frac{dx}{dt} =
#' 0.05(90 - 0.5g_\mathrm{Ca}(1 + \mathrm{tanh}(x + 1.2)/18))(x - 120) -
#' 8y(x + 84) - 2(x + 60),}}
#' \ifelse{html}{\out{<i>dy</i>/<i>dt</i> = <i>&phi;</i>(0.5(1 +
#' tanh((<i>x</i> - 2)/30)) - <i>y</i>)cosh((<i>x</i> -
#' 2)/60).}}{\deqn{\frac{dy}{dt} = \phi(0.5\left[1 +
#' \mathrm{tanh}\left(\frac{x - 2}{30}\right)\right] - y)\mathrm{cosh}(\frac{x -
#' 2}{60}).}}
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
#' \ifelse{html}{\out{<i>g</i><sub>Ca</sub>}}{\eqn{g_\mathrm{Ca}}},
#' \ifelse{html}{\out{<i>&phi;</i>}}{\eqn{\phi}}.
#' @return Returns a \code{\link[base]{list}} containing the values of the two
#' derivatives at
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}.
#' @author Michael J Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @export
morrisLecar <- function(t, y, parameters){
  list(c((1/20)*(90 - parameters[1]*0.5*(1 + tanh((y[1] + 1.2)/18))*
                   (y[1] - 120) - 8*y[2]*(y[1] + 84) - 2*(y[1] + 60)),
         parameters[2]*(0.5*(1 + tanh((y[1] - 2)/30)) - y[2])*
           cosh((y[1] - 2)/60)))
}
