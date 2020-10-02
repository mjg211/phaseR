#' Nullclines
#'
#' Plots nullclines for two-dimensional autonomous ODE systems. Can also be used
#' to plot horizontal lines at equilibrium points for one-dimensional autonomous
#' ODE systems.
#'
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required structure of these
#' functions can be found in the package vignette, or in the help file for the
#' function \code{\link[deSolve]{ode}}.
#' @param xlim In the case of a two-dimensional system, this sets the limits of
#' the first dependent variable in which gradient reflecting line segments
#' should be plotted. In the case of a one-dimensional system, this sets the
#' limits of the independent variable in which these line segments should be
#' plotted. Should be a \code{\link[base]{numeric}} \code{\link[base]{vector}}
#' of \code{\link[base]{length}} two.
#' @param ylim In the case of a two-dimensional system this sets the limits of
#' the second dependent variable in which gradient reflecting line segments
#' should be plotted. In the case of a one-dimensional system, this sets the
#' limits of the dependent variable in which these line segments should be
#' plotted. Should be a \code{\link[base]{numeric}} \code{\link[base]{vector}}
#' of \code{\link[base]{length}} two.
#' @param parameters Parameters of the ODE system, to be passed to \code{deriv}.
#' Supplied as a \code{\link[base]{numeric}} \code{\link[base]{vector}}; the
#' order of the parameters can be found from the \code{deriv} file. Defaults to
#' \code{NULL}.
#' @param points Sets the density at which derivatives are computed;
#' \code{points} x \code{points} derivatives will be computed. Levels of zero
#' gradient are identified using these computations and the function
#' \code{\link[graphics]{contour}}. Increasing the value of points improves
#' identification of nullclines, but increases computation time. Defaults to
#' \code{101}.
#' @param system Set to either \code{"one.dim"} or \code{"two.dim"} to indicate
#' the type of system being analysed. Defaults to \code{"two.dim"}.
#' @param col In the case of a two-dimensional system, sets the colours used
#' for the x- and y-nullclines. In the case of a one-dimensional system, sets
#' the colour of the lines plotted horizontally along the equilibria. Should be
#' a \code{\link[base]{character}} \code{\link[base]{vector}} of
#' \code{\link[base]{length}} two. Will be reset accordingly if it is of the
#' wrong \code{\link[base]{length}}. Defaults to \code{c("blue", "cyan")}.
#' @param add Logical. If \code{TRUE}, the nullclines are added to an existing
#' plot. If \code{FALSE}, a new plot is created. Defaults to \code{TRUE}.
#' @param add.legend Logical. If \code{TRUE}, a \code{\link[graphics]{legend}}
#' is added to the plots. Defaults to \code{TRUE}.
#' @param \dots Additional arguments to be passed to either
#' \code{\link[graphics]{plot}} or \code{\link[graphics]{contour}}.
#' @inheritParams .paramDummy
#' @return Returns a \code{\link[base]{list}} with the following components (the
#' exact make up is dependent on the value of \code{system}):
#' \item{add}{As per input.}
#' \item{add.legend}{As per input.}
#' \item{col}{As per input, but with possible editing if a
#' \code{\link[base]{character}} \code{\link[base]{vector}} of the wrong
#' \code{\link[base]{length}} was supplied.}
#' \item{deriv}{As per input.}
#' \item{dx}{A \code{\link[base]{numeric}} \code{\link[base]{matrix}}. In the
#' case of a two-dimensional system, the values of the derivative of the first
#' dependent derivative at all evaluated points.}
#' \item{dy}{A \code{\link[base]{numeric}} \code{\link[base]{matrix}}. In the
#' case of a two-dimensional system, the values of the derivative of the second
#' dependent variable at all evaluated points. In the case of a one-dimensional
#' system, the values of the derivative of the dependent variable at all
#' evaluated points.}
#' \item{parameters}{As per input.}
#' \item{points}{As per input.}
#' \item{system}{As per input.}
#' \item{x}{A \code{\link[base]{numeric}} \code{\link[base]{vector}}. In the
#' case of a two-dimensional system, the values of the first dependent variable
#' at which the derivatives were computed. In the case of a one-dimensional
#' system, the values of the independent variable at which the derivatives were
#' computed.}
#' \item{xlim}{As per input.}
#' \item{y}{A \code{\link[base]{numeric}} \code{\link[base]{vector}}. In the
#' case of a two-dimensional system, the of values of the second dependent
#' variable at which the derivatives were computed. In the case of a
#' one-dimensional system, the values of the dependent variable at which the
#' derivatives were computed.}
#' \item{ylim}{As per input.}
#' @note In order to ensure a nullcline is plotted, set \code{xlim} and
#' \code{ylim} strictly enclosing its location. For example, to ensure a
#' nullcline is plotted along x = 0, set \code{ylim} to, e.g., begin at -1.
#' @author Michael J Grayling
#' @seealso \code{\link[graphics]{contour}}, \code{\link[graphics]{plot}}
#' @export
#' @examples
#' # Plot the flow field, nullclines and several trajectories for the
#' # one-dimensional autonomous ODE system logistic.
#' logistic_flowField  <- flowField(logistic,
#'                                  xlim       = c(0, 5),
#'                                  ylim       = c(-1, 3),
#'                                  parameters = c(1, 2),
#'                                  points     = 21,
#'                                  system     = "one.dim",
#'                                  add        = FALSE)
#' logistic_nullclines <- nullclines(logistic,
#'                                   xlim       = c(0, 5),
#'                                   ylim       = c(-1, 3),
#'                                   parameters = c(1, 2),
#'                                   system     = "one.dim")
#' logistic_trajectory <- trajectory(logistic,
#'                                   y0         = c(-0.5, 0.5, 1.5, 2.5),
#'                                   tlim       = c(0, 5),
#'                                   parameters = c(1, 2),
#'                                   system     = "one.dim")
#'
#' # Plot the velocity field, nullclines and several trajectories for the
#' # two-dimensional autonomous ODE system simplePendulum.
#' simplePendulum_flowField  <- flowField(simplePendulum,
#'                                        xlim       = c(-7, 7),
#'                                        ylim       = c(-7, 7),
#'                                        parameters = 5,
#'                                        points     = 19,
#'                                        add        = FALSE)
#' y0                        <- matrix(c(0, 1, 0, 4, -6, 1, 5, 0.5, 0, -3),
#'                                     5, 2, byrow = TRUE)
#' \donttest{
#' simplePendulum_nullclines <- nullclines(simplePendulum,
#'                                         xlim       = c(-7, 7),
#'                                         ylim       = c(-7, 7),
#'                                         parameters = 5,
#'                                         points     = 500)
#' }
#' simplePendulum_trajectory <- trajectory(simplePendulum,
#'                                         y0         = y0,
#'                                         tlim       = c(0, 10),
#'                                         parameters = 5)
nullclines <- function(deriv, xlim, ylim, parameters = NULL,
                       system = "two.dim", points = 101,
                       col = c("blue", "cyan"), add = TRUE, add.legend = TRUE,
                       state.names =
                         if (system == "two.dim") c("x", "y") else "y", ...) {
  if (any(!is.vector(xlim), length(xlim) != 2)) {
    stop("xlim is not a vector of length 2, as is required")
  }
  if (xlim[2] <= xlim[1]) {
    stop("xlim[2] is less than or equal to xlim[1]")
  }
  if (any(!is.vector(ylim), length(ylim) != 2)) {
    stop("ylim is not a vector of length 2, as is required")
  }
  if (ylim[2] <= ylim[1]) {
    stop("ylim[2] is less than or equal to ylim[1]")
  }
  if (points <= 0) {
    stop("points is less than or equal to zero")
  }
  if (!(system %in% c("one.dim", "two.dim"))) {
    stop("system must be set to either \"one.dim\" or \"two.dim\"")
  }
  if (!is.vector(col)) {
    stop("col is not a vector as required")
  }
  if (length(col) != 2) {
    if (length(col) == 1) {
      col      <- rep(col, 2)
    } else if (length(col) > 2) {
      col      <- col[1:2]
    }
    message("Note: col has been reset as required")
  }
  if (!is.logical(add)) {
    stop("add must be logical")
  }
  if (!is.logical(add.legend)) {
    stop("add.legend must be logical")
  }
  x            <- seq(xlim[1], xlim[2], length.out = points)
  y            <- seq(ylim[1], ylim[2], length.out = points)
  dx           <- dy <- matrix(0, ncol = points, nrow = points)
  if (system == "one.dim") {
    for (i in 1:points) {
      dy[1, i] <- deriv(0, stats::setNames(c(y[i]), state.names),
                        parameters)[[1]]
    }
    for (i in 2:points) {
      dy[i, ]  <- dy[1, ]
    }
    graphics::contour(x, y, dy, levels = 0, add = add, col = col[1],
                      drawlabels = F, ...)
    if (add.legend) {
      graphics::legend("bottomright",
                       paste0("d", state.names, "/dt = 0 for all t"), lty = 1,
                       lwd = 1, col = col[1])
    }
    return(list(add        = add,
                add.legend = add.legend,
                col        = col,
                deriv      = deriv,
                dy         = dy,
                parameters = parameters,
                points     = points,
                system     = system,
                x          = x,
                xlim       = xlim,
                y          = y,
                ylim       = ylim))
  } else {
    for (i in 1:points) {
      for (j in 1:points) {
        df       <- deriv(0, stats::setNames(c(x[i], y[j]), state.names),
                          parameters)
        dx[i, j] <- df[[1]][1]
        dy[i, j] <- df[[1]][2]
      }
    }
    graphics::contour(x, y, dx, levels = 0, add = add, col = col[1],
                      drawlabels = F, ...)
    graphics::contour(x, y, dy, levels = 0, add = T, col = col[2],
                      drawlabels = F, ...)
    if (add.legend) {
      legend("bottomright", paste(state.names, "nullclines"), lty = 1,
             lwd = 1, col = col)
    }
    return(list(add        = add,
                add.legend = add.legend,
                col        = col,
                deriv      = deriv,
                dx         = dx,
                dy         = dy,
                parameters = parameters,
                points     = points,
                system     = system,
                x          = x,
                xlim       = xlim,
                y          = y,
                ylim       = ylim))
  }
}


nullclines2 <- function(plot = NULL, deriv, xlim, ylim, parameters = NULL,
                        system = "two.dim", state.names = "default", points = 100,
                        return.type = c("plot", "df", "all"),
                        color = c("blue", "cyan"), linewidth = 1,
                        add.legend = TRUE) {

  return.type <- match.arg(return.type)

  if (any(!is.vector(xlim), length(xlim) != 2)) {
    stop("xlim is not a vector of length 2, as is required")
  }
  if (xlim[2] <= xlim[1]) {
    stop("xlim[2] is less than or equal to xlim[1]")
  }
  if (any(!is.vector(ylim), length(ylim) != 2)) {
    stop("ylim is not a vector of length 2, as is required")
  }
  if (ylim[2] <= ylim[1]) {
    stop("ylim[2] is less than or equal to ylim[1]")
  }
  if (points <= 0) {
    stop("points is less than or equal to zero")
  }
  if (!(system %in% c("one.dim", "two.dim"))) {
    stop("system must be set to either \"one.dim\" or \"two.dim\"")
  }
  if (!is.vector(color)) {
    stop("color is not a vector as required")
  }

  if (system == "one.dim") color <- color[1]
  if (system == "two.dim") {
    if (length(color) == 1) color <- rep(color, 2) else color <- color[1:2]
  }
  if (!is.logical(add.legend)) {
    stop("add.legend must be logical")
  }
  if (state.names == "default") {
    state.names <- if (system == "two.dim") c("x", "y") else "y"
  }
  if (system == "one.dim") {
    stopifnot(length(state.names) == 1)
  } else if (system == "two.dim") {
    stopifnot(length(state.names) == 2)
  }

  if (system == "one.dim") {
    # It is a little complicated, but basically it's trying to avoid
    # contour-like computations since once we find the zero we know
    # the nullcline is represented by a horizontal line.
    y <- seq(ylim[1], ylim[2], length.out = points)
    dy <- vector("numeric", points)
    for (idx in seq_along(dy)) {
      state <- y[idx]
      names(state) <- state.names
      dy[idx] <- deriv(0, state, parameters)[[1]]
    }

    # change has two consecutive equal values when there is an exact zero.
    changes <- diff(sign(dy))
    idx_remove <- vector("numeric")
    for (i in 1L:(length(changes) - 1)) {
      if (changes[i] == changes[i + 1] && changes[i] != 0) {
        idx_remove <- c(idx_remove, i, i + 1)
      }
    }
    if (length(idx_remove) > 0) {
      # if there are exact zeros...
      changes <- changes[-idx_remove]
      y0_exact <- y[which(dy == 0)]
    } else {
      y0_exact <- c()
    }

    # If change has non-zero values, we find approximate nullclines
    idx <- which(changes != 0)

    if (length(idx) > 0 ) {
      y <- sapply(idx, find_zeros, y = y, deriv = deriv,
                  state.names = state.names, parameters = parameters)
      y <- c(y, y0_exact)
    } else {
      y <- y0_exact
    }

    df <- data.frame(
      x = rep(c(xlim[1], xlim[2]), length(y)),
      y = rep(y, each = 2),
      id = rep(paste0("group_", seq_along(y)), each = 2),
      state = state.names[1]
    )
    df$group <- interaction(df$id, df$state)

  } else {
    x <- rep(seq(xlim[1], xlim[2], length.out = points), times = points)
    y <- rep(seq(ylim[1], ylim[2], length.out = points), each = points)
    dx <- dy <- vector("numeric", points ^ 2)
    for (idx in seq_along(dx)) {
      state <- c(x[idx], y[idx])
      names(state) <- state.names
      dxdy = deriv(0, state, parameters)
      dx[idx] = dxdy[[1]][1]
      dy[idx] = dxdy[[1]][2]
    }

    df <- data.frame(x = x, y = y, z = dx)
    iso <- xyz_to_isolines(df, 0)

    df_path <- data.frame(
      x = unlist(lapply(iso, "[[", "x"), use.names = FALSE),
      y = unlist(lapply(iso, "[[", "y"), use.names = FALSE),
      id = unlist(lapply(iso, "[[", "id"), use.names = FALSE),
      state = state.names[1]
    )

    df <- data.frame(x = x, y = y, z = dy)
    iso <- xyz_to_isolines(df, 0)

    df <- rbind(
      df_path,
      data.frame(
        x = unlist(lapply(iso, "[[", "x"), use.names = FALSE),
        y = unlist(lapply(iso, "[[", "y"), use.names = FALSE),
        id = unlist(lapply(iso, "[[", "id"), use.names = FALSE),
        state = state.names[2]
      )
    )
    df$group <- interaction(df$id, df$state)
  }

  if (is.null(plot)) {
    plot <- ggplot2::ggplot(df) +
      ggplot2::geom_path(
        mapping = ggplot2::aes(x = x, y = y, group = group, color = state)
      ) +
      ggplot2::scale_color_manual(values = color)
  } else {
    if (!inherits(plot, "ggplot")) {
      stop("`plot` must be a ggplot2 plot not of class '", class(plot), "'")
    }
    plot <- plot +
      ggplot2::geom_path(
        mapping = ggplot2::aes(x = x, y = y, group = group, color = state),
        data = df
      ) +
      ggplot2::scale_color_manual(values = color)
  }

  # Remove legend if desired
  if (!add.legend) plot <- plot + ggplot2::theme(legend.position = "none")
  if (return.type == "plot") {
    return(plot)
  } else if (return.type == "df") {
    return(df)
  } else {
    return(list("df" = df, "plot" = plot))
  }

}

isoband_z_matrix <- function(data) {
  # Convert vector of data to raster
  x_pos <- as.integer(factor(data$x, levels = sort(unique(data$x))))
  y_pos <- as.integer(factor(data$y, levels = sort(unique(data$y))))

  nrow <- max(y_pos)
  ncol <- max(x_pos)

  raster <- matrix(NA_real_, nrow = nrow, ncol = ncol)
  raster[cbind(y_pos, x_pos)] <- data$z
  raster
}

xyz_to_isolines <- function(data, breaks) {
  isoband::isolines(
    x = sort(unique(data$x)),
    y = sort(unique(data$y)),
    z = isoband_z_matrix(data),
    levels = breaks
  )
}

find_zeros <- function(idx, y, deriv, state.names, parameters) {
  y <- seq(y[idx], y[idx + 1], length.out = round(length(y) / 2))
  dy <- vector("numeric", length(y))
  for (idx in seq_along(dy)) {
    state <- y[idx]
    names(state) <- state.names
    dy[idx] <- deriv(0, state, parameters)[[1]]
  }
  changes <- diff(sign(dy))
  idx <- which(changes != 0)
  y <- (y[idx] + y[idx + 1]) / 2
  return(y)
}
