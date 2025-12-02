FitGompertz <- function(gdat) {
  
  # Negative log-likelihood (because nlminb minimizes)
  gompertz_nll <- function(par, dat) {
    
    # Extract parameters
    b0  <- par[1]
    b1  <- par[2]
    g0  <- par[3]
    g1  <- par[4]
    logb <- par[5]
    log_sig2 <- par[6]
    
    # Transformations
    b    <- exp(logb)
    sig2 <- exp(log_sig2)
    
    # Guard: sigma must not be absurdly small
    sig2 <- max(sig2, 1e-10)
    
    # Pre-extract data
    X    <- dat$X
    tpts <- dat$tpts
    Y    <- dat$Y
    
    # Compute log(a) and log(c)
    log_a <- b0 + b1 * X
    log_c <- g0 + g1 * X
    
    # Clip so exp(log_a / log_c) cannot overflow
    log_a <- pmin(log_a, 700)
    log_c <- pmin(log_c, 700)
    
    # Compute c = exp(log_c)
    cVal <- exp(log_c)
    
    # Inner exponent: -c * t
    inner <- -cVal * tpts
    inner <- pmax(inner, -700)   # prevent exp(inner) underflow
    
    # Middle exponent: -b * exp(inner)
    middle <- -b * exp(inner)
    middle <- pmax(middle, -700) # prevent exp(middle) underflow
    
    # mu = exp(log_a + middle)
    mu <- log_a + middle
    mu <- pmin(mu, 700)          # prevent overflow in exp(mu)
    mu <- exp(mu)
    
    # squared error
    sq <- (Y - mu)^2
    
    # Negative log-likelihood
    n <- length(Y)
    nll <- (n/2)*log(sig2) + (1/(2*sig2)) * sum(sq)
    
    # Fail-safe: return large penalty if bad value appears
    if (!is.finite(nll)) return(1e50)
    
    return(nll)
  }
  
  
  # Starting parameters (log-scale for b and sig2)
  startPar <- c(
    b0 = 2,
    b1 = 0.25,
    g0 = 0.25,
    g1 = 0.25,
    logb = log(7),
    log_sig2 = log(0.25)
  )
  
  # Wide bounds allowed because log scale is safe
  lowerBound <- c(-50, -50, -50, -50, log(1e-8), log(1e-8))
  upperBound <- c(50, 50, 50, 50, log(1e6), log(1e8))
  
  # nlminb is MUCH more stable for these kinds of problems
  fit <- nlminb(
    start = startPar,
    objective = gompertz_nll,
    dat = gdat,
    lower = lowerBound,
    upper = upperBound,
    control = list(eval.max = 5000, iter.max = 5000)
  )
  
  return(fit$par[1:4])
}