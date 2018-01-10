#' The Lotka Volterra Model
#' 
#' The derivative function of the Lotka Volterra model, an example of a two
#' dimensional autonomous ODE system.
#' 
#' Evaluates the derivative of the following coupled ODE system at the point
#' (t, x, y):
#' 
#' dx/dt = lambda*x - epsilon*x*y, dy/dt = eta*x*y - delta*y.
#' 
#' Format is designed to be compatible with ode from the deSolve package.
#' 
#' @param t Value of t, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y Values of x and y, the dependent variables, to evaluate the
#' derivative at. Should be a vector of length 2.
#' @param parameters Values of the parameters of the system. Should be a vector
#' with parameters specified in the following order: lambda, epsilon, eta,
#' delta.
#' @return Returns a list dy containing the values of the two derivatives at
#' (t, x, y).
#' @author Michael J. Grayling
#' @export
#' @seealso \code{\link{ode}}
lotkaVolterra <- function(t, y, parameters){
    x <- y[1]
    y <- y[2]
    lambda  <- parameters[1]
    epsilon <- parameters[2]
    eta     <- parameters[3]
    delta   <- parameters[4]
    dy    <- numeric(2)
    dy[1] <- lambda*x - epsilon*x*y
    dy[2] <- eta*x*y - delta*y
    list(dy)
}
