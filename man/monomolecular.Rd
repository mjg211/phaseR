\name{monomolecular}
\alias{monomolecular}
\title{The Monomolecular Growth Model}
\description{The derivative function of the monomolecular growth model, an example of a one dimensional autonomous ODE system.}
\usage{monomolecular(t, y, parameters)}
\arguments{
  \item{t}{Value of t, the independent variable, to evaluate the derivative at. Should be a single number.}
  \item{y}{Value of y, the dependent variable, to evaluate the derivative at. Should be a single number.}
  \item{parameters}{Values of the parameters of the system. Should be a vector with parameters specified in the following order: beta, K.}
}
\details{Evaluates the derivative of the following ODE at the point (t, y):

dy/dt = beta*(K - y)

Format is designed to be compatible with ode from the deSolve package.}
\value{Returns a list dy containing the value of the derivative at (t, y).}
\author{Michael J. Grayling}
\seealso{\code{\link{ode}}}