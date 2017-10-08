morrisLecar <- function(t, y, parameters){
  x <- y[1]
  y <- y[2]
  gca <- parameters[1]
  phi <- parameters[2]
  dy    <- numeric(2)
  dy[1] <- (1/20)*(90 - gca*0.5*(1 + tanh((x + 1.2)/18))*(x - 120) -
                     8*y*(x + 84) - 2*(x + 60))
  dy[2] <- phi*(0.5*(1 + tanh((x - 2)/30)) - y)*cosh((x - 2)/60)
  list(dy)
}