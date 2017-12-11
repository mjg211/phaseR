#' The Simple Pendulum
#' 
#' The derivative function of the Simple Pendulum, an example of a two
#' dimensional autonomous ODE system.
#' 
#' Evaluates the derivative of the following coupled ODE system at the point
#' (t, x, y):
#' 
#' dx/dt = y, dy/dt = -g*sin(x)/l.
#' 
#' Format is designed to be compatible with ode from the deSolve package.
#' 
#' @param t Value of t, the independent variable, to evaluate the derivative
#' at. Should be a single number.
#' @param y Values of x and y, the dependent variables, to evaluate the
#' derivative at. Should be a vector of length 2.
#' @param parameters Values of the parameters of the system. Should be a number
#' for the value of alpha. g is automatically entered as 9.81.
#' @return Returns a list dy containing the values of the two derivatives at
#' (t, x, y).
#' @author Michael J. Grayling
#' @seealso \code{\link{ode}}
#' @export
#' @examples
#' # Plot the velocity field, nullclines and several trajectories.
#' simplePendulum.flowField  <- flowField(simplePendulum, xlim = c(-7, 7),
#'                                        ylim = c(-7, 7), parameters = 5, points = 19,
#'                                        add = FALSE)
#' y0                        <- matrix(c(0, 1, 0, 4, -6, 1, 5, 0.5, 0, -3), ncol = 2,
#'                                     nrow = 5, byrow = TRUE)
#' simplePendulum.nullclines <- nullclines(simplePendulum, xlim = c(-7, 7),
#'                                         ylim = c(-7, 7), parameters = 5, points = 500)
#' simplePendulum.trajectory <- trajectory(simplePendulum, y0 = y0, tlim = c(0, 10),
#'                                         parameters = 5)
#' simplePendulum.manifolds  <- drawManifolds(simplePendulum, y0 = c(pi, 0),
#'                                            parameters = 5, tend = 1000,
#'                                            col = c("green", "red"), add.legend = TRUE)
#' 
#' # Determine the stability of two of the equilibrium points.
#' simplePendulum.stability.1 <- stability(simplePendulum, ystar = c(0, 0),
#'                                         parameters = 5)
#' simplePendulum.stability.2 <- stability(simplePendulum, ystar = c(pi, 0),
#'                                         parameters = 5)
#' 
simplePendulum <- function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  g <- 9.81
  l <- parameters[1]
  dy    <- numeric(2)
  dy[1] <- y
  dy[2] <- -g*sin(x)/l
  list(dy)
}
