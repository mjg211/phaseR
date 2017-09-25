\name{flowField}
\alias{flowField}
\title{Flow Field}
\description{Plots the flow or velocity field for a one or two dimensional autonomous ODE system.}
\usage{flowField(deriv, xlim, ylim, parameters = NULL, points = 11, system = "two.dim",
                 col = "gray", arrow.type = "equal", arrow.head = 0.05, frac = 1,
				 add = TRUE, xlab = "x", ylab = "y", ...)}
\arguments{
  \item{deriv}{A function computing the derivative at a point for the ODE system to be analysed. Discussion of the required format of these functions can be found in the package guide.}
  \item{xlim}{In the case of a two dimensional system, this sets the limits of the first dependent variable in which gradient reflecting line segments should be plotted. In the case of a one dimensional system, this sets the limits of the independent variable in which these line segments should be plotted. Should be a vector of length two.}
  \item{ylim}{In the case of a two dimensional system this sets the limits of the second dependent variable in which gradient reflecting line segments should be plotted. In the case of a one variable system, this sets the limits of the dependent variable in which these line segments should be plotted. Should be a vector of length two.}
  \item{parameters}{Parameters of the ODE system, to be passed to deriv. Supplied as a vector; the order of the parameters can be found from the deriv file. Defaults to NULL.}
  \item{points}{Sets the density of the line segments to be plotted. points segments will be plotted in the x and y directions. Fine tuning here, by shifting points up and down, allows for the creation of more aesthetically pleasing plots. Defaults to 11.}
  \item{system}{Set to either "one.dim" or "two.dim" to indicate the type of system being analysed. Defaults to "two.dim".}
  \item{col}{Sets the colour of the plotted line segments. Should be a vector of length one. Will be reset accordingly if it is a vector of the wrong length. Defaults to "gray".}
  \item{arrow.type}{Sets the type of line segments plotted. If set to "proportional" the length of the line segments reflects the magnitude of the derivative. If set to "equal" the line segments take equal lengths, simply reflecting the gradient of the derivative(s). Defaults to "equal".}
  \item{arrow.head}{Sets the length of the arrow heads. Passed to arrows. Defaults to 0.05.}
  \item{frac}{Sets the fraction of the theoretical maximum length line segments can take without overlapping, that they can actually attain. In practice, frac can be set to greater than 1 without line segments overlapping. Fine tuning here assists the creation of aesthetically pleasing plots. Defaults to 1.}
  \item{add}{Logical. If TRUE, the flow field is added to an existing plot. If FALSE, a new plot is created. Defaults to TRUE.}
  \item{xlab}{Label for the x-axis of the resulting plot. Defaults to "x".}
  \item{ylab}{Label for the y-axis of the resulting plot. Defaults to "y".}
  \item{\dots}{Additional arguments to be passed to either plot or arrows.}
}
\value{Returns a list with the following components (the exact make up is dependent upon the value of system):
  \item{add}{As per input.}
  \item{arrow.head}{As per input.}
  \item{arrow.type}{As per input.}
  \item{col}{As per input, but with possible editing if a vector of the wrong length was supplied.}
  \item{deriv}{As per input.}
  \item{dx}{A matrix. In the case of a two dimensional system, the values of the derivative of the first dependent derivative at all evaluated points.}
  \item{dy}{A matrix. In the case of a two dimensional system, the values of the derivative of the second dependent variable at all evaluated points. In the case of a one dimensional system, the values of the derivative of the dependent variable at all evaluated points.}
  \item{frac}{As per input.}
  \item{parameters}{As per input.}
  \item{points}{As per input.}
  \item{system}{As per input.}
  \item{x}{A vector. In the case of a two dimensional system, the values of the first dependent variable at which the derivatives were computed. In the case of a one dimensional system, the values of the independent variable at which the derivatives were computed.}
  \item{xlab}{As per input.}
  \item{xlim}{As per input.}
  \item{y}{A vector. In the case of a two dimensional system, the values of the second dependent variable at which the derivatives were computed. In the case of a one dimensional system, the values of the dependent variable at which the derivatives were computed.}
  \item{ylab}{As per input.}
  \item{ylim}{As per input.}
}
\author{Michael J. Grayling}
\seealso{\code{\link{arrows}}, \code{\link{plot}}}
\examples{# Plot the flow field, nullclines and several trajectories for the one
# dimensional autonomous ODE system logistic.
logistic.flowField <- flowField(logistic, xlim = c(0, 5), ylim = c(-1, 3),
                                parameters = c(1, 2), points = 21, system = "one.dim",
								add = FALSE)
logistic.nullclines <- nullclines(logistic, xlim = c(0, 5), ylim = c(-1, 3),
                                  parameters = c(1, 2), system = "one.dim")
logistic.trajectory <- trajectory(logistic, y0 = c(-0.5, 0.5, 1.5, 2.5), tlim = c(0, 5),
                                  parameters = c(1, 2), system = "one.dim")

# Plot the velocity field, nullclines and several trajectories for the two dimensional
# autonomous ODE system simplePendulum.
simplePendulum.flowField  <- flowField(simplePendulum, xlim = c(-7, 7),
                                       ylim = c(-7, 7), parameters = 5, points = 19,
									   add = FALSE)
y0                        <- matrix(c(0, 1, 0, 4, -6, 1, 5, 0.5, 0, -3), ncol = 2,
                                    nrow = 5, byrow = TRUE)
simplePendulum.nullclines <- nullclines(simplePendulum, xlim = c(-7, 7),
                                        ylim = c(-7, 7), parameters = 5, points = 500)
simplePendulum.trajectory <- trajectory(simplePendulum, y0 = y0, tlim = c(0, 10),
                                        parameters = 5)
}