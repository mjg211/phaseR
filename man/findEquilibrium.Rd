\name{findEquilibrium}
\alias{findEquilibrium}
\title{Equilibrium Point Identification}
\description{Searches for an equilibium point of a system, taking the starting point of the search as a user specified point. On identifying such a point, a classification is performed, and an informatively shaped point can be added to the plot.}
\usage{findEquilibrium(deriv, y0 = NULL, parameters = NULL,
                              system = "two.dim", tol = 1e-16,
                              max.iter = 50, h = 1e-6, plot.it = FALSE,   
                              summary = TRUE)}
\arguments{
  \item{deriv}{A function computing the derivative at a point for the ODE system to be analysed. Discussion of the required structure of these functions can be found in the package guide.}
  \item{y0}{The starting point of the search. In the case of a one dimensional system, this should be a single number indicating the location of the dependent variable initially. In the case of a two dimensional system, this should be a vector of length two reflecting the location of the two dependent variables initially. Alternatively this can be left blank and the user can use locator to specify initial condition on a plot. Defaults to NULL.}
  \item{parameters}{Parameters of the ODE system, to be passed to deriv. Supplied as a vector; the order of the parameters can be found from the deriv file. Defaults to NULL.}
  \item{system}{Set to either "one.dim" or "two.dim" to indicate the type of system being analysed. Defaults to "two.dim".}
  \item{tol}{The tolerance for the convergence of the search algorithm. Defaults to 1e-16.}
  \item{max.iter}{The maximum allowed number of iterations of the search algorithm. Defaults to 50.}
  \item{h}{Step length used to approximate the derivative(s). Defaults to 1e-6.}
  \item{plot.it}{Logical. If TRUE, a point is plotted at the identified equilibrium point, with shape corresponding to its classification.}
  \item{summary}{Set to either TRUE or FALSE to determine whether a summary of the progress of the search procedure is returned. Defaults to TRUE.}
}
\value{Returns a list with the following components (the exact make up is dependent upon the value of system):
  \item{classification}{The classification of the identified equilibrium point.}
  \item{Delta}{In the two dimensional system case, value of the Jacobians determinant at the equilibrium point.}
  \item{deriv}{As per input.}
  \item{discriminant}{In the one dimensional system case, the value of the discriminant used in Perturbation Analysis to assess stability. In the two dimensional system case, the value of T^2 - 4*Delta.}
  \item{eigenvalues}{In the two dimensional system case, the value of the Jacobians eigenvalues at the equilibrium point.}
  \item{eigenvectors}{In the two dimensional system case, the value of the Jacobians eigenvectors at the equilibrium point.}
  \item{jacobian}{In the two dimensional system case, the Jacobian at the equilibrium point.}
  \item{h}{As per input.}
  \item{max.iter}{As per input.}
  \item{parameters}{As per input.}
  \item{plot.it}{As per input.}
  \item{summary}{As per input.}
  \item{system}{As per input.}
  \item{tr}{In the two dimensional system case, the value of the Jacobians trace at the equilibrium point.}
  \item{tol}{As per input.}
  \item{y0}{As per input.}
  \item{ystar}{The location of the identified equilibrium point.}
}
\author{Michael J. Grayling, Stephen P. Ellner, John M. Guckenheimer}
\examples{}