\name{example14}
\alias{example14}
\title{Example ODE System Number Fourteen}
\description{The derivative function of an example two dimensional autonomous ODE system.}
\usage{example14(t, y, parameters)}
\arguments{
  \item{t}{Value of t, the independent variable, to evaluate the derivative at. Should be a single number.}
  \item{y}{Values of x and y, the dependent variables, to evaluate the derivative at. Should be a vector of length 2.}
  \item{parameters}{Values of the parameters of the system. Not required here.}
}
\details{Evaluates the derivative of the following coupled ODE system at the point (t, x, y):

dx/dt = x^2 - y - 10,   dy/dt = -3x^2 + x*y.

Format is designed to be compatible with ode from the deSolve package.}
\value{Returns a list dy containing the values of the two derivatives at (t, x, y).}
\author{Michael J. Grayling}
\seealso{\code{\link{ode}}}