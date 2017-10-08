toggle <- function(t, y, parameters){
  x  <- y[1]
  y  <- y[2]
  alpha <- parameters[1]
  beta  <- parameters[2]
  gamma <- parameters[3]
  dy    <- numeric(2)
  dy[1] <- -x + alpha/(1 + y^beta)
  dy[2] <- -y + alpha/(1 + x^gamma)
  list(dy) 
}