GenerateGompertz <- function(N=1000,                                        
                             P=15,                                        
                             beta = c(2,0.25),                                     
                             gamma = c(0.25,0.25),                                    
                             b = 7,                                        
                             sigma = 0.25){    
  #browser()
  X <- cbind(1, rnorm(N))                                                       
  a <- exp(X%*%beta)                                                            
  c <- exp(X%*%gamma)                                                           
  b <- exp(b)                                                                   
  dat <- lapply(1:N, function(i){                                               
    # Generate time points                                                      
    tpts <- 1:P                                                                 
    # Compute means                                                             
    mu <- a[i]*exp(-b*exp(-c[i]*tpts))                                          
    # Generate Y                                                                
    Y <- rnorm(rep(1, length(mu)), mu, sd = sigma)                              
    
    # Output data set                                                           
    list(ID = rep(i,P),                                                         
         X = rep(X[i,2],P),                                                    
         tpts = tpts,                                                          
         Y = Y)                                                                
  })                                                                            
  out <- data.frame(ID = unlist(lapply(dat, function(x){x$ID})),                
                    X = unlist(lapply(dat, function(x){x$X})),                  
                    tpts = unlist(lapply(dat, function(x){x$tpts})),            
                    Y = unlist(lapply(dat, function(x){x$Y})))                  
  out                                                                           
}       

dat <- GenerateGompertz(N = 100)

# Plot several example curves:
par(mfrow = c(2,2))
for (i in sample(1:100, size = 4)){
  keep <- dat$ID == i
  plot(dat$tpts[keep], dat$Y[keep], type = "b", 
       main = paste0("Curve for Subject ", i), xlab = "Time", ylab = "Y")
}



# install.packages("minpack.lm")
# library(minpack.lm)
# DNase1 <- subset(DNase, Run == 1)
# fm1DNase1 <- nlsLM(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
# coef(fm1DNase1)
# logLik(fm1DNase1)
# TT <- seq(0, 8, length = 501)
# tau <- 2.2
# N0 <- 1000
# a <- 0.25
# f0 <- 8
# Ndet <- N0 * exp(-TT/tau) * (1 + a * cos(f0 * TT))
# N <- Ndet +  rnorm(length(Ndet), mean = Ndet, sd = .01 * max(Ndet))
# library(minpack.lm)
# gform <- Y ~ exp(b0 + b1*X) * exp( - b * exp( - exp(g0 + g1*X) * tpts ) )
# m1 <- nlsLM(gform, data = dat)
# coef(m1)[names(coef(m1)) != "b"]
# mean(fitted(m1)[1:15] - dat[1:15,4])
# 
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



b0 <- 2
b1 <- .25
g0 <- .25
g1 <- .25
b <- 7
sig2 <- 1



n <- nrow(dat)
subHolder <- rep(NA, n)
for (i in 1:n) {
  a_i <- aFun(x_i = dat$X[i], b0 = b0, b1 = b1)
  c_i <- cFun(x_i = dat$X[i], g0 = g0, g1 = g1)
  mu_ij <- muFun(a_i = a_i, c_i = c_i, b = b, t_ij = dat$tpts[i])
  subHolder[i] <- (dat$Y[i] - mu_ij)^2
}
l <- -(n/2)*log(sig2) - (1/(2*sig2)) * sum(subHolder)


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
par <- c(b0, b1, g0, g1, b, sig2)
toOptim(par, dat)




dat <- GenerateGompertz(N=1000,                                        
                        P=15,                                        
                        beta = c(2,2),                                     
                        gamma = c(2,.22),                                    
                        b = 7,                                        
                        sigma = 2)
startPar <- c(b0 = 1, b1 = 1, g0 = 1, g1 = 1, b = 1, sig2 = 1)
fit <- optim(
  par = startPar,
  fn = toOptim,
  dat = dat,  
  lower = c(b0 = 1e-8, b1 = 1e-8, g0 = 1e-8, g1 = 1e-8, b = 1e-8, sig2 = 1e-8),
  upper = c(b0 = Inf, b1 = Inf, g0 = Inf, g1 = Inf, b = Inf, sig2 = Inf),
  method = "L-BFGS-B",
  control = list(fnscale = -1, maxit = 5000)  
)
fit$par[1:4]
#does his function do sigma or sig^2? does it matter?


dat <- GenerateGompertz(N=1000,                                        
                        P=10,                                        
                        beta = c(2,2),                                     
                        gamma = c(2,2),                                    
                        b = 7,                                        
                        sigma = .25)
FitGompertz(gdat = dat)





FitGompertz2 <- function(gdat) {
  #par is the parameter vector b0, b1, g0, g1, b, sig2
  toOptim <- function(par, dat) {
    b0 <- par[1]
    b1 <- par[2]
    g0 <- par[3]
    g1 <- par[4]
    b <- exp(par[5])
    sig2 <- par[6]
    
    
    n <- nrow(dat)
    
    
    aVec <- exp(b0+b1*dat$X)
    cVec <- exp(g0+g1*dat$X)
    muVec <- aVec * exp(-b * exp(-cVec * dat$tpts))
    subSquare <- (dat$Y-muVec)^2
    l <- -(n/2)*log(sig2) - (1/(2*sig2)) * sum(subSquare)
    return(-l)
  }
  
  
  startPar <- c(b0 = 2, b1 = .25, g0 = .25, g1 = .25, b = 7, sig2 = .25)
  lowerBound <- c(b0 = 1e-8, b1 = 1e-8, g0 = 1e-8, g1 = 1e-8, b = 1e-8, sig2 = 1e-8)
  upperBound <- c(b0 = 10, b1 = 10, g0 = 10, g1 = 10, b = 15, sig2 = 10)
  # fit <- optim(
  #   par = startPar,
  #   fn = toOptim,
  #   dat = gdat,  
  #   lower = lowerBound,
  #   upper = upperBound,
  #   method = "L-BFGS-B",
  #   control = list(fnscale = -1, maxit = 5000)  
  # )
  
  
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
  
  
  
  return(fit2$par)
}
 
FitGompertz2(gdat = dat)


       