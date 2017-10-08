\name{example7}
\alias{example7}
\title{Example ODE System Number Seven}
\description{The derivative function of an example two dimensional autonomous ODE system.}
\usage{example7(t, y, parameters)}
\arguments{
  \item{t}{Value of t, the independent variable, to evaluate the derivative at. Should be a single number.}
  \item{y}{Values of x and y, the dependent variables, to evaluate the derivative at. Should be a vector of length 2.}
  \item{parameters}{Values of the parameters of the system. Not required here.}
}
\details{Evaluates the derivatives of the following coupled ODE system at the point (t, x, y):

dx/dt = -x - y,   dy/dt = 4*x + y.

Format is designed to be compatible with ode from the deSolve package.}
\value{Returns a list dy containing the value of the derivatives at (t, x, y).}
\author{Michael J. Grayling}
\seealso{\code{\link{ode}}}