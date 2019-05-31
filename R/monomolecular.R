#' The monomolecular growth model
#' 
#' The derivative function of the monomolecular growth model, an example of a
#' one-dimensional autonomous ODE system.
#' 
#' \code{monomolecular} evaluates the derivative of the following ODE at the
#' point \ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}:
#' 
#' \ifelse{html}{\out{<center><i>dy</i>/<i>dt</i> = <i>&beta;</i>(<i>K</i> -
#' <i>y</i>).</center>}}{\deqn{\frac{dy}{dt} = \beta(K - y).}}
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
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}},
#' \ifelse{html}{\out{<i>K</i>}}{\eqn{K}}.
#' @return Returns a \code{\link[base]{list}} containing the value of the
#' derivative at \ifelse{html}{\out{(<i>t</i>, <i>y</i>)}}{\eqn{(t, y)}}.
#' @author Michael J Grayling
#' @seealso \code{\link[deSolve]{ode}}
#' @export
monomolecular <- function(t, y, parameters){
  list(parameters[1]*(parameters[2] - y))
}
