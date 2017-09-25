\name{vonBertalanffy}
\alias{vonBertalanffy}
\title{The von Bertalanffy Growth Model}
\description{The derivative function of the von Bertalanffy growth model, an example of a one dimensional autonomous ODE system.}
\usage{vonBertalanffy(t, y, parameters)}
\arguments{
  \item{t}{Value of t, the independent variable, to evaluate the derivative at. Should be a single number.}
  \item{y}{Value of y, the dependent variable, to evaluate the derivative at. Should be a single number.}
  \item{parameters}{Values of the parameters of the system. Should be a vector with parameters specified in the following order: alpha, beta.}
}
\details{Evaluates the derivative of the following ODE at the point (t, y):

dy/dt = alpha*y^(2/3) - beta*y.

Format is designed to be compatible with ode from the deSolve package.}
\value{Returns a list dy containing the value of the derivative at (t, y).}
\author{Michael J. Grayling}
\seealso{\code{\link{ode}}}