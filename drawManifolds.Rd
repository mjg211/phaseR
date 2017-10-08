\name{drawManifolds}
\alias{drawManifolds}
\title{Stable and Unstable Manifolds}
\description{Plots the stable and unstable manifolds of a saddle point. A search procedure is utilised to identify an equilibrium point, and if this is a saddle then the manifolds are added to the plot.}
\usage{drawManifolds(deriv, y0 = NULL, parameters = NULL, tend = 1000,
              col = c("green", "red"), add.legend = TRUE, ...)}
\arguments{
  \item{deriv}{A function computing the derivative at a point for the ODE system to be analysed. Discussion of the required structure of these functions can be found in the package guide.}
  \item{y0}{The initial point from which a saddle will be searched for. This can either be a vector of length two reflecting the location of the two dependent variables initially, or it can be left blank and the user can use locator to specify the initial point on a plot}
  \item{parameters}{Parameters of the ODE system, to be passed to deriv. Supplied as a vector; the order of the parameters can be found from the deriv file. Defaults to NULL.}
  \item{tend}{The final time of the numerical integration performed to identify the manifolds.}
  \item{col}{Sets the colours used for the stable and unstable manifolds. Will be reset accordingly if it is a vector of the wrong length. Defaults to c("green", "red").}
  \item{add.legend}{Logical. If TRUE, a legend is added to the plots. Defaults to TRUE.}
  \item{...}{Additional arguments to be passed to plot.}
}
\value{Returns a list with the following components (the exact make up is dependent upon the value of system):
  \item{add.legend}{As per input.}
  \item{col}{As per input, but with possible editing if a vector of the wrong length was supplied.}
  \item{deriv}{As per input.}
  \item{parameters}{As per input.}
  \item{stable.1}{A matrix whose columns are the numerically computed values of the dependent variables for part of the stable manifold.}
  \item{stable.2}{A matrix whose columns are the numerically computed values of the dependent variables for part of the stable manifold.}
  \item{tend}{As per input.}
  \item{unstable.1}{A matrix whose columns are the numerically computed values of the dependent variables for part of the unstable manifold.}
  \item{unstable.2}{A matrix whose columns are the numerically computed values of the dependent variables for part of the unstable manifold.}
  \item{y0}{As per input.}
  \item{ystar}{Location of the identified equilibrium point.}
}
\author{Michael J. Grayling, Stephen P. Ellner, John M. Guckenheimer}
\examples{}