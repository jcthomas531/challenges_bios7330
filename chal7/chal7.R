
FitGompertz <- function(gdat) {
  #par is the parameter vector b0, b1, g0, g1, b, sig2
  toOptim <- function(par, dat) {
    b0 <- par[1]
    b1 <- par[2]
    g0 <- par[3]
    g1 <- par[4]
    b <- exp(par[5])
    sig2 <- par[6]
    # Guard: sigma must not be absurdly small
    sig2 <- max(sig2, 1e-10)
    
    
    
    
    n <- nrow(dat)
    
    
    #trying on log scale with catches
    log_a <- b0+b1*dat$X
    log_c <- g0+g1*dat$X
    #gaurd agains values that are too big
    log_a <- pmin(log_a, 700)
    log_c <- pmin(log_c, 700)
    
    
    
    #do all of this in small steps with catches all along the way
    p1 <- -exp(log_c) * dat$tpts
    p1 <- pmax(p1, -700)
    
    p2 <- -b * exp(p1)
    p2 <- pmax(p2, -700)
    
    p3 <- log_a + p2
    p3 <- pmin(p3, 700)
    
    muVec <- exp(p3)
    
    
    # aVec <- exp(b0+b1*dat$X)
    # cVec <- exp(g0+g1*dat$X)
    # muVec <- aVec * exp(-b * exp(-cVec * dat$tpts))
    
    
    
    subSquare <- (dat$Y-muVec)^2
    l <- -(n/2)*log(sig2) - (1/(2*sig2)) * sum(subSquare)
    
    if(!is.finite(l)) {
      return(1e50)
      } 
    
    
    return(l)
  }
  
  
  startPar <- c(b0 = 1, b1 = 1, g0 = 1, g1 = 1, b = 7, sig2 = .25)
  lowerBound <- c(b0 = 1e-8, b1 = 1e-8, g0 = 1e-8, g1 = 1e-8, b = 1e-8, sig2 = 1e-8)
  upperBound <- c(b0 = 10, b1 = 10, g0 = 10, g1 = 10, b = 15, sig2 = 10)
  fit2 <- nlminb(start = startPar, objective = toOptim, dat = gdat, 
                 control = list(eval.max = 5000, iter.max = 5000),
                 lower = lowerBound, upper = upperBound
  )
  
  # if (fit$convergence != "0") {
  #   stop(paste0("converg code:0", fit$convergence, ". ", fit$message))
  # }
  # 
  # #check if on the edge
  # edgeClose <- abs(fit$par - upperBound)
  # if (edgeClose < 1) {
  #   stop(paste0(which.min(edgeClose)), " is too close to boundary: ", min(edgeClose))
  # }
  
  
  
  return(fit2$par[1:4])
}
