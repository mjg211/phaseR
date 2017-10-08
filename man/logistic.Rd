\name{logistic}
\alias{logistic}
\title{The Logistic Growth Model}
\description{The derivative function of the logistic growth model, an example of a two dimensional autonomous ODE system.}
\usage{logistic(t, y, parameters)}
\arguments{
  \item{t}{Value of t, the independent variable, to evaluate the derivative at. Should be a single number.}
  \item{y}{Value of y, the dependent variable, to evaluate the derivative at. Should be a single number.}
  \item{parameters}{Values of the parameters of the system. Should be a vector with parameters specified in the following order: beta, K.}
}
\details{Evaluates the derivative of the following ODE at the point (t, y):

dy/dt = beta*y*(1 - y/K).

Format is designed to be compatible with ode from the deSolve package.}
\value{Returns a list dy containing the value of the derivative at (t, y).}
\author{Michael J. Grayling}
\seealso{\code{\link{ode}}}
\examples{# Plot the velocity field, nullclines and several trajectories.
logistic.flowField  <- flowField(logistic, xlim = c(0, 5), ylim = c(-1, 3),
                                 parameters = c(1, 2), points = 21, system = "one.dim",
								                 add = FALSE)
logistic.nullclines <- nullclines(logistic, xlim = c(0, 5), ylim = c(-1, 3),
                                  parameters = c(1, 2), system = "one.dim")
logistic.trajectory <- trajectory(logistic, y0 = c(-0.5, 0.5, 1.5, 2.5), tlim = c(0, 5),
                                  parameters = c(1, 2), system = "one.dim")

# Plot the phase portrait.
logistic.phasePortrait <- phasePortrait(logistic, ylim = c(-0.5, 2.5),
                                        parameters = c(1, 2), points = 10, frac = 0.5)

# Determine the stability of the equilibrium points.
logistic.stability.1 <- stability(logistic, ystar = 0, parameters = c(1, 2),
                                  system = "one.dim")
logistic.stability.2 <- stability(logistic, ystar = 2, parameters = c(1, 2),
                                  system = "one.dim")
}