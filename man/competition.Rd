\name{competition}
\alias{competition}
\title{Species Competition Model}
\description{The derivative function of the species competition model, an example of a two dimensional autonomous ODE system.}
\usage{competition(t, y, parameters)}
\arguments{
  \item{t}{Value of t, the independent variable, to evaluate the derivative at. Should be a single number.}
  \item{y}{Values of x and y, the dependent variables, to evaluate the derivative at. Should be a vector of length 2.}
  \item{parameters}{Values of the parameters of the system. Should be a vector with parameters specified in the following order: r1, K1, alpha12, r2, K2, alpha21.}
}
\details{Evaluates the derivative of the following coupled ODE system at the point (t, x, y):

dx/dt = r1*x*(K1 - x - alpha12*y)/K1,   dy/dt = r2*x*(K2 - x - alpha21*y)/K2.

Format is designed to be compatible with ode from the deSolve package.}
\value{Returns a list dy containing the values of the two derivatives at (t, x, y).}
\author{Michael J. Grayling}
\seealso{\code{\link{ode}}}