#his code:
P <- matrix(c(0.25,0.25,0.5,
              0,0.5,0.5,
              0.5,0,0.5), 
            nrow = 3, 
            byrow=TRUE)
X <- integer(50000)
X[1] <- 1
for (i in 2:length(X)){
  X[i] <- which(
    rmultinom(1, 1, prob = P[X[i-1],]) > 0)
}
# After chain approximately converges, what are the estimated
# state probabilities?


#the section of the chain after the chain has converges (probably, ie the second half)
X_converged <- X[round(length(X)/2):length(X)]
#proportion of elements in the converged chain that are in each state
sapply(1:3, function(i){mean(X_converged == i)})


#https://stephens999.github.io/fiveMinuteStats/stationary_distribution.html
#Since every state is accessible from every other state, this Markov chain is irreducible.
#Every irreducible finite state space Markov chain has a unique stationary distribution.
#Recall that the stationary distribution π is the row vector such that pi = pi P
#where P is the transition matrix
#Thus we can find the stationary distribution by solving a system of linear equations
#Since this linear system has more equations than unknowns, it is an overdetermined 
#system. Overdetermined systems can be solved using a QR decomposition, so we use that here.


#input
P <- matrix(c(0.25,0.25,0.5,
              0,0.5,0.5,
              0.5,0,0.5), 
            nrow = 3, 
            byrow=TRUE)
#extract needed vals
dimP <- nrow(P)
#set up linear system
A <- rbind(t(P - diag(dimP)), #transition matrix with vall vars moved over
           rep(1, dimP)) #restriction
b <- c(rep(0, dimP), 1)
qr.solve(A, b, tol = 1e-3)


get_stationary_dist <- function(P) {
  #extract needed vals
  dimP <- nrow(P)
  #set up linear system
  A <- rbind(t(P - diag(dimP)), #transition matrix with vall vars moved over
             rep(1, dimP)) #restriction
  b <- c(rep(0, dimP), 1)
  #solve system
  return(qr.solve(A, b, tol = 1e-3)) #tolerance could be changed for more exact results
}



P1 <- matrix(c(0.7,0.4,0,0.2,0.6,1,0.1,0,0), nrow = 3)
get_stationary_dist(P1)
P2 <- matrix(c(0.25,0.25,0.5,
               0,0.5,0.5,
               0.5,0,0.5), 
             nrow = 3, 
             byrow=TRUE)
get_stationary_dist(P2)


#try on random matrix
#install.packages("LaplacesDemon")
k <- 10
P <- LaplacesDemon::rdirichlet(k, alpha = c(1:k))
get_stationary_dist(P)


