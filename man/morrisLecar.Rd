\name{morrisLecar}
\alias{morrisLecar}
\title{Morris-Lecar Model}
\description{The derivative function of the Morris-Lecar model, an example of a two dimensional autonomous ODE system.}
\usage{morrisLecar(t, y, parameters)}
\arguments{
  \item{t}{Value of t, the independent variable, to evaluate the derivative at. Should be a single number.}
  \item{y}{Values of x and y, the dependent variables, to evaluate the derivative at. Should be a vector of length 2.}
  \item{parameters}{Values of the parameters of the system. Should be a vector with parameters specified in the following order: gca, phi.}
}
\details{Evaluates the derivative of the following coupled ODE system at the point (t, x, y):

dx/dt = (1/20)*(90 - gca*0.5*(1 + tanh((x + 1.2)/18))*(x - 120) -
                  8*y*(x + 84) - 2*(x + 60)),
dy/dt = phi*(0.5*(1 + tanh((x - 2)/30)) - y)*cosh((x - 2)/60).

Format is designed to be compatible with ode from the deSolve package.}
\value{Returns a list dy containing the values of the two derivatives at (t, x, y).}
\author{Michael J. Grayling, Stephen P. Ellner, John M. Guckenheimer}
\seealso{\code{\link{ode}}}