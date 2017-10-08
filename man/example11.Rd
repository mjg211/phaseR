\name{example11}
\alias{example11}
\title{Example ODE System Number Eleven}
\description{The derivative function of an example two dimensional autonomous ODE system.}
\usage{example11(t, y, parameters)}
\arguments{
  \item{t}{Value of t, the independent variable, to evaluate the derivative at. Should be a single number.}
  \item{y}{Values of x and y, the dependent variables, to evaluate the derivative at. Should be a vector of length 2.}
  \item{parameters}{Values of the parameters of the system. Not required here.}
}
\details{Evaluates the derivative of the following coupled ODE system at the point (t, x, y):

dx/dt = x(3 - x - 2*y),   dy/dt = y(2 - x - y).

Format is designed to be compatible with ode from the deSolve package.}
\value{Returns a list dy containing the values of the two derivatives at (t, x, y).}
\author{Michael J. Grayling}
\seealso{\code{\link{ode}}}
\examples{# Plot the velocity field, nullclines and several trajectories.
example11.flowField  <- flowField(example11, xlim = c(-5, 5), ylim = c(-5, 5),
                                  points = 21, add = FALSE)
y0                   <- matrix(c(4, 4, -1, -1, -2, 1, 1, -1), ncol = 2, nrow = 4,
                               byrow = TRUE)
example11.nullclines <- nullclines(example11, xlim = c(-5, 5), ylim = c(-5, 5),
                                   points = 200)
example11.trajectory <- trajectory(example11, y0 = y0, tlim = c(0, 10))
example11.manifolds  <- drawManifolds(example11, y0 = c(1, 1), tend = 1000,
                                      col = c("green", "red"), add.legend = TRUE)

# Determine the stability of the equilibrium points.
example11.stability.1 <- stability(example11, ystar = c(0, 0))
example11.stability.2 <- stability(example11, ystar = c(0, 2))
example11.stability.3 <- stability(example11, ystar = c(1, 1))
example11.stability.4 <- stability(example11, ystar = c(3, 0))
}