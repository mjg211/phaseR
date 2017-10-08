\name{example1}
\alias{example1}
\title{Example ODE System Number One}
\description{The derivative function of an example one dimensional autonomus ODE system.}
\usage{example1(t, y, parameters)}
\arguments{
  \item{t}{Value of t, the independent variable, to evaluate the derivative at. Should be a single number.}
  \item{y}{Value of y, the dependent variable, to evaluate the derivative at. Should be a single number.}
  \item{parameters}{Values of the parameters of the system. Not required here.}
}
\details{Evaluates the derivative of the following ODE at the point (t, y):

dy/dt = 4 - y^2.

Format is designed to be compatible with ode from the deSolve package.}
\value{Returns a list dy containing the value of the derivative at (t, y).}
\author{Michael J. Grayling}
\seealso{\code{\link{ode}}}