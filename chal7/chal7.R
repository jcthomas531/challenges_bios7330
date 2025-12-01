
FitGompertz <- function(gdat) {
  #
  aFun <- function(x_i, b0, b1) {
    exp(b0+b1*x_i)
  }
  cFun <- function(x_i, g0, g1) {
    exp(g0+g1*x_i)
  }
  muFun <- function(a_i,c_i,b, t_ij) {
    a_i* exp(-b * exp(-c_i * t_ij))
  }
  
  
  #par is the parameter vector b0, b1, g0, g1, b, sig2
  toOptim <- function(par, dat) {
    b0 <- par[1]
    b1 <- par[2]
    g0 <- par[3]
    g1 <- par[4]
    b <- par[5]
    sig2 <- par[6]
    
    
    n <- nrow(dat)
    subHolder <- rep(NA, n)
    for (i in 1:n) {
      a_i <- aFun(x_i = dat$X[i], b0 = b0, b1 = b1)
      c_i <- cFun(x_i = dat$X[i], g0 = g0, g1 = g1)
      mu_ij <- muFun(a_i = a_i, c_i = c_i, b = b, t_ij = dat$tpts[i])
      subHolder[i] <- (dat$Y[i] - mu_ij)^2
    }
    l <- -(n/2)*log(sig2) - (1/(2*sig2)) * sum(subHolder)
    return(l)
  }
  
  
  startPar <- c(b0 = 1, b1 = 1, g0 = 1, g1 = 1, b = 1, sig2 = 1)
  fit <- optim(
    par = startPar,
    fn = toOptim,
    dat = gdat,  
    lower = c(b0 = 1e-8, b1 = 1e-8, g0 = 1e-8, g1 = 1e-8, b = 1e-8, sig2 = 1e-8),
    upper = c(b0 = Inf, b1 = Inf, g0 = Inf, g1 = Inf, b = Inf, sig2 = Inf),
    method = "L-BFGS-B",
    control = list(fnscale = -1, maxit = 5000)  
  )
  return(fit$par[1:4])
}
