phasePlaneAnalyser <- function(deriv, xlim, ylim, tend = 100,
                               parameters = NULL, system = "two.dim",  
                               add = FALSE){
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
  menu.go <- 1
  all.j   <- NULL
  while (menu.go > 0){
    jl <- utils::select.list(c("1: Flow field", 
                        "2: Nullclines",
                        "3: Find fixed point (click on plot)",
                        "4: Start Forward trajectory (click on plot)",
                        "5: Start Backward trajectory (click on plot)",
                        "6: Extend Current trajectory (a trajectory must already be plotted)",
                        "7: Local S/U manifolds of a saddle (two dimensional systems only) (click on plot)",
                        "8: Grid of trajectories",
                        "9: Exit",
                        "10: Save plot as PDF"),
                      title = "Phase Plane Analyser: Select Action")
    j     <- substr(jl, 1, 2)
    all.j <- c(all.j, j)
    if (j == "1:"){
      flow.field <- flowField(deriv = deriv, xlim = xlim, ylim = ylim,
                              parameters = parameters, system = system, add = add)
      add        <- TRUE
    } else if (j == "2:"){
      null.clines <- nullclines(deriv = deriv, xlim = xlim, ylim = ylim, points = 500,
                                parameters = parameters, system = system, add = add)
      add         <- TRUE
    } else if (j == "3:"){
      if (add == FALSE){
        print("To identify and plot an equilibrium point you must first choose an option that initialises a plot")
      } else {
        ystar <- findEquilibrium(deriv = deriv, parameters = parameters,
                                 system = system, plot.it = TRUE)
      }
    } else if (j == "4:"){
      if (add == FALSE){
        print("To plot a trajectory you must first choose an option that initialises a plot")
      } else {
        traj <- trajectory(deriv = deriv, n = 1, tlim = c(0, tend),
                           tstep = 0.01, parameters = parameters,
                           system = system)
      }
    } else if (j == "5:"){
      if (add == FALSE){
        print("To plot a trajectory you must first choose an option that initialises a plot")
      } else {
        traj <- trajectory(deriv = deriv, n = 1, tlim = c(0, -tend),
                           tstep = -0.01, parameters = parameters,
                           system = system, col = "brown")
      }
    } else if (j == "6:"){
      if (any(all.j %in% c("4:", "5:"))){
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
                            system = system, col = col)
      } else {
        print("To extend a trajectory one must already have been plotted")
      }
    } else if (j == "7:"){
      if (system == "one.dim"){
        print("Draw manifolds option is available only for two dimensional systems")
      } else {
        if (add == FALSE){
          print("To identify and plot the manifolds you must first choose an option that initialises a plot")
        } else {
          manifolds <- drawManifolds(deriv = deriv, parameters = parameters,
                                     tend = tend)
        }
      }
    } else if (j == "8:"){
      if (add == FALSE){
        print("To identify and plot trajectories you must first choose an option that initialises a plot")
      } else {
        if (system == "one.dim"){
          y0 <- seq(from = ylim[1], to = ylim[2], length.out = 8)
        } else {
          x  <- seq(from = xlim[1], to = xlim[2], length.out = 4)
          y  <- seq(from = ylim[1], to = ylim[2], length.out = 4)
          y0 <- matrix(0, 16, 2)
          for (i in 1:4){
            y0[(1 + 4*(i - 1)):(4*i), ] <- cbind(x, rep(y[i], 4))
          }
          y0 <- y0[-c(4, 16), ]
        }
        traj.grid <- trajectory(deriv = deriv, y0 = y0, tlim = c(0, tend),
                                parameters = parameters, system = system,
                                col = rep("black", nrow(y0)))
      }
    } else if (j == "9:"){
      menu.go <- 0
    } else if (j == "10"){
      grDevices::dev.copy2pdf(file = "phasePlaneAnalysis.pdf")
    }
  }
}