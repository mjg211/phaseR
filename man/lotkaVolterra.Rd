\name{lotkaVolterra}
\alias{lotkaVolterra}
\title{The Lotka Volterra Model}
\description{The derivative function of the Lotka Volterra model, an example of a two dimensional autonomous ODE system.}
\usage{lotkaVolterra(t, y, parameters)}
\arguments{
  \item{t}{Value of t, the independent variable, to evaluate the derivative at. Should be a single number.}
  \item{y}{Values of x and y, the dependent variables, to evaluate the derivative at. Should be a vector of length 2.}
  \item{parameters}{Values of the parameters of the system. Should be a vector with parameters specified in the following order: lambda, epsilon, eta, delta.}
}
\details{Evaluates the derivative of the following coupled ODE system at the point (t, x, y):

dx/dt = lambda*x - epsilon*x*y,   dy/dt = eta*x*y - delta*y.

Format is designed to be compatible with ode from the deSolve package.}
\value{Returns a list dy containing the values of the two derivatives at (t, x, y).}
\author{Michael J. Grayling}
\seealso{\code{\link{ode}}}