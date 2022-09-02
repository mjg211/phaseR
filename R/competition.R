#' The species competition model
#'
#' The derivative function of the species competition model, an example of a
#' two-dimensional autonomous ODE system.
#'
#' \code{competition} evaluates the derivative of the following coupled ODE
#' system at the point \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{
#' \eqn{(t, x, y)}}:
#'
#' \ifelse{html}{\out{<i>dx</i>/<i>dt</i> =
#' <i>r</i><sub>1</sub><i>x</i>(<i>K</i><sub>1</sub> - <i>x</i> -
#' <i>&alpha;</i><sub>12</sub><i>y</i>)/<i>K</i><sub>1</sub>,
#' <i>dy</i>/<i>dt</i> = <i>r</i><sub>2</sub><i>y</i>(<i>K</i><sub>2</sub> -
#' <i>y</i> - <i>&alpha;</i><sub>21</sub><i>x</i>)/<i>K</i><sub>2</sub>.}}{\deqn{\frac{dx}{dt} = r_1x(K_1 - x - \alpha_{12}y)/K_1,
#' \frac{dy}{dt} = r_2y(K_2 - y - \alpha_{21}x)/K_2.}}
#'
#' Its format is designed to be compatible with \code{\link[deSolve]{ode}} from
#' the \code{\link[deSolve]{deSolve}} package.
#'
#' @param t The value of \ifelse{html}{\out{<i>t</i>}}{\eqn{t}}, the independent
#' variable, to evaluate the derivative at. Should be a
#' \code{\link[base]{numeric}} \code{\link[base]{vector}} of
#' \code{\link[base]{length}} one.
#' @param y The values of \ifelse{html}{\out{<i>x</i>}}{\eqn{x}} and
#' \ifelse{html}{\out{<i>y</i>}}{\eqn{y}}, the dependent
#' variables, to evaluate the derivative at. Should be a
#' \code{\link[base]{numeric}} \code{\link[base]{vector}} of
#' \code{\link[base]{length}} one.
#' @param parameters The values of the parameters of the system. Should be a
#' \code{\link[base]{numeric}} \code{\link[base]{vector}} with parameters
#' specified in the following order:
#' \ifelse{html}{\out{<i>r</i><sub>1</sub>}}{\eqn{r_1}},
#' \ifelse{html}{\out{<i>K</i><sub>1</sub>}}{\eqn{K_1}},
#' \ifelse{html}{\out{<i>&alpha;</i><sub>12</sub>}}{\eqn{\alpha_{12}}},
#' \ifelse{html}{\out{<i>r</i><sub>2</sub>}}{\eqn{r_2}},
#' \ifelse{html}{\out{<i>K</i><sub>2</sub>}}{\eqn{K_2}},
#' \ifelse{html}{\out{<i>&alpha;</i><sub>21</sub>}}{\eqn{\alpha_{21}}}.
#' @return Returns a \code{\link[base]{list}} containing the values of the two
#' derivatives at
#' \ifelse{html}{\out{(<i>t</i>, <i>x</i>, <i>y</i>)}}{\eqn{(t, x, y)}}.
#' @author Michael J Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @export
competition <- function(t, y, parameters) {
  list(c(parameters[1]*y[1]*(parameters[2] - y[1] -
                               parameters[3]*y[2])/parameters[2],
         parameters[4]*y[2]*(parameters[5] - y[2] -
                               parameters[6]*y[1])/parameters[5]))
}
