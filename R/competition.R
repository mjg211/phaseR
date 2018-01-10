#' Species Competition Model
#' 
#' The derivative function of the species competition model, an example of a
#' two dimensional autonomous ODE system.
#' 
#' Evaluates the derivative of the following coupled ODE system at the point
#' (t, x, y):
#' 
#' dx/dt = r1*x*(K1 - x - alpha12*y)/K1, dy/dt = r2*x*(K2 - x - alpha21*y)/K2.
#' 
#' Format is designed to be compatible with ode from the deSolve package.
#' 
#' @param t Value of t, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y Values of x and y, the dependent variables, to evaluate the
#' derivative at. Should be a vector of length 2.
#' @param parameters Values of the parameters of the system. Should be a vector
#' with parameters specified in the following order: r1, K1, alpha12, r2, K2,
#' alpha21.
#' @return Returns a list dy containing the values of the two derivatives at
#' (t, x, y).
#' @author Michael J. Grayling
#' @seealso \code{\link{ode}}
#' @export
competition <- function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  r1      <- parameters[1]
  K1      <- parameters[2]
  alpha12 <- parameters[3]
  r2      <- parameters[4]
  K2      <- parameters[5]
  alpha21 <- parameters[6]
  dy    <- numeric(2)
  dy[1] <- r1*x*(K1 - x - alpha12*y)/K1
  dy[2] <- r2*y*(K2 - y - alpha21*x)/K2
  list(dy)
}
