#' Phase Plane Analysis
#' 
#' Allows the user to perform a basic phase plane analysis and produce a simple
#' plot without the need to use the other functions directly. Specifically, a
#' list is provided and the user inputs a value to the console to decide what
#' is added to the plot.
#' 
#' The user designates the derivative file and other arguments as per the
#' above. Then the following 10 options are available for execution:
#' 
#' \itemize{
#' \item 1. Flow field: Plots the flow field of the system. See
#' \code{\link{flowField}}.
#' \item 2. Nullclines: Plots the nullclines of the system. See
#' \code{\link{nullclines}}.
#' \item 3. Find fixed point (click on plot): Searches for an equilibrium point
#' of the system, taking the starting point of the search as where the user
#' clicks on the plot. See \code{\link{findEquilibrium}}.
#' \item 4. Start Forward trajectory (click on plot): Plots a trajectory, i.e.,
#' a solution, forward in time with the starting point taken as where the user
#' clicks on the plot. See \code{\link{trajectory}}.
#' \item 5. Start Backward trajectory (click on plot): Plots a trajectory, i.e.,
#' a solution, backward in time with the starting point taken as where the user
#' clicks on the plot. See \code{\link{trajectory}}.
#' \item 6. Extend Current trajectory (a trajectory must already be plotted):
#' Extends already plotted trajectories on further in time. See
#' \code{\link{trajectory}}.
#' \item 7.  Local S/U manifolds of a saddle (two dimensional systems only)
#' (click on plot): Plots the stable and unstable manifolds of a saddle point.
#' The user clicks on the plot and an equilibrium point is identified (see 3
#' above), if this point is a saddle then the manifolds are plotted. See
#' \code{\link{drawManifolds}}.
#' \item 8. Grid of trajectories: Plots a set of trajectories, with the starting
#' points defined on an equally spaced grid over the designated plotting range
#' for the dependent variable(s). See \code{\link{trajectory}}.
#' \item 9.  Exit: Exits the current call to phasePlaneAnalyser().
#' \item 10. Save plot as PDF: Saves the produced plot as
#' "phasePlaneAnalysis.pdf" in the current working directory.
#' }
#' 
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required structure of these
#' functions can be found in the package guide.
#' @param xlim In the case of a two dimensional system, this sets the limits of
#' the first dependent variable in any subsequent plot. In the case of a one
#' dimensional system, this sets the limits of the independent variable. Should
#' be a vector of length two.
#' @param ylim In the case of a two dimensional system this sets the limits of
#' the second dependent variable in any subsequent plot. In the case of a
#' dimensional system, this sets the limits of the dependent variable. Should
#' be a vector of length two.
#' @param tend The value of the independent variable to end any subsequent
#' numerical integrations at.
#' @param parameters Parameters of the ODE system, to be passed to deriv.
#' Supplied as a vector; the order of the parameters can be found from the
#' deriv file. Defaults to NULL.
#' @param system Set to either "one.dim" or "two.dim" to indicate the type of
#' system being analysed. Defaults to "two.dim".
#' @param add Logical. If TRUE, the chosen features are added to an existing
#' plot. If FALSE, a new plot is created. Defaults to FALSE.
#' @inheritParams .paramDummy
#' @author Michael J. Grayling, Stephen P. Ellner, John M. Guckenheimer
#' @export
phasePlaneAnalysis <- function(deriv, xlim, ylim, tend = 100,
                               parameters = NULL, system = "two.dim",
                               add = FALSE, state.names = if(system == "two.dim") c("x", "y") else "y") {
  if ((!is.vector(xlim)) | (length(xlim) != 2)){
    stop("xlim is not a vector of length 2 as required")
  }
  if (xlim[2] <= xlim[1]){
    stop("xlim[2] is less than or equal to xlim[1]")
  }
  if ((!is.vector(ylim)) | (length(ylim) != 2)){
    stop("ylim is not a vector of length 2 as required")
  }
  if (ylim[2] <= ylim[1]){
    stop("ylim[2] is less than or equal to ylim[1]")
  }
  if (!(system %in% c("one.dim", "two.dim"))){
    stop("system must either be set to \"one.dim\" or \"two.dim\"")
  }
  if (!is.logical(add)){
    stop("add must be set to TRUE or FALSE")
  }
  if (tend <= 0){
    stop("tend must be strictly positive")
  }
  
  actions <- c("Flow field", 
               "Nullclines",
               "Find fixed point (click on plot)",
               "Start Forward trajectory (click on plot)",
               "Start Backward trajectory (click on plot)",
               "Extend Current trajectory (a trajectory must already be plotted)",
               "Local S/U manifolds of a saddle (two dimensional systems only) (click on plot)",
               "Grid of trajectories",
               "Exit",
               "Save plot as PDF")
  
  menu.go <- 1
  all.j   <- NULL
  while (menu.go > 0){
    jl <- utils::select.list(actions, title = "Phase Plane Analyser: Select Action")
    if(jl == "") break # menu cancelled (0)
    j     <- match(jl, actions)
    all.j <- c(all.j, j)
    if (j == 1){
      flow.field <- flowField(deriv = deriv, xlim = xlim, ylim = ylim,
                              parameters = parameters, system = system, add = add, state.names = state.names)
      add        <- TRUE
    } else if (j == 2){
      null.clines <- nullclines(deriv = deriv, xlim = xlim, ylim = ylim, points = 500,
                                parameters = parameters, system = system, add = add, state.names = state.names)
      add         <- TRUE
    } else if (j == 3){
      if (add == FALSE){
        print("To identify and plot an equilibrium point you must first choose an option that initialises a plot")
      } else {
        ystar <- findEquilibrium(deriv = deriv, parameters = parameters,
                                 system = system, plot.it = TRUE, state.names = state.names)
      }
    } else if (j == 4){
      if (add == FALSE){
        print("To plot a trajectory you must first choose an option that initialises a plot")
      } else {
        traj <- trajectory(deriv = deriv, n = 1, tlim = c(0, tend),
                           tstep = 0.01, parameters = parameters,
                           system = system, state.names = state.names)
      }
    } else if (j == 5){
      if (add == FALSE){
        print("To plot a trajectory you must first choose an option that initialises a plot")
      } else {
        traj <- trajectory(deriv = deriv, n = 1, tlim = c(0, -tend),
                           tstep = -0.01, parameters = parameters,
                           system = system, col = "brown", state.names = state.names)
      }
    } else if (j == 6){
      if (any(all.j %in% c(4, 5))){
        t     <- traj$t
        tstep <- t[2] - t[1]
        y0    <- traj$y[length(t)]
        x0    <- NULL
        if (system == "two.dim"){
          x0  <- traj$x[length(t)]
        }
        col   <- ifelse(tstep > 0, "black", "brown")
        traj  <- trajectory(deriv = deriv, y0 = c(x0, y0),
                            tlim = c(t[1], t[length(t)]),
                            tstep = tstep, parameters = parameters,
                            system = system, col = col, state.names = state.names)
      } else {
        print("To extend a trajectory one must already have been plotted")
      }
    } else if (j == 7){
      if (system == "one.dim"){
        print("Draw manifolds option is available only for two dimensional systems")
      } else {
        if (add == FALSE){
          print("To identify and plot the manifolds you must first choose an option that initialises a plot")
        } else {
          manifolds <- drawManifolds(deriv = deriv, parameters = parameters,
                                     tend = tend, state.names = state.names)
        }
      }
    } else if (j == 8){
      if (add == FALSE){
        print("To identify and plot trajectories you must first choose an option that initialises a plot")
      } else {
        if (system == "one.dim"){
          y0 <- seq(from = ylim[1], to = ylim[2], length.out = 8)
          times = length(y0) 
        } else {
          x  <- seq(from = xlim[1], to = xlim[2], length.out = 4)
          y  <- seq(from = ylim[1], to = ylim[2], length.out = 4)
          y0 <- matrix(0, 16, 2)
          for (i in 1:4){
            y0[(1 + 4*(i - 1)):(4*i), ] <- cbind(x, rep(y[i], 4))
          }
          y0 <- y0[-c(4, 16), ]
          times = nrow(y0) 
        }
        traj.grid <- trajectory(deriv = deriv, y0 = y0, tlim = c(0, tend),
                                parameters = parameters, system = system,
                                col = rep("black", times), state.names = state.names)
      }
    } else if (j == 9){
      menu.go <- 0
    } else if (j == 10){
      grDevices::dev.copy2pdf(file = "phasePlaneAnalysis.pdf")
    } else {
      # Should not happen
      stop("select.list problem")
    }
    
  }
}
