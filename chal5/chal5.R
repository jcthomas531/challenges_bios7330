#2025_11_10
#https://stephens999.github.io/fiveMinuteStats/stationary_distribution.html
#Since every state is accessible from every other state, this Markov chain is irreducible.
#Every irreducible finite state space Markov chain has a unique stationary distribution.
#Recall that the stationary distribution π is the row vector such that pi = pi P
#where P is the transition matrix
#Thus we can find the stationary distribution by solving a system of linear equations
#Since this linear system has more equations than unknowns, it is an overdetermined 
#system. Overdetermined systems can be solved using a QR decomposition, so we use that here.
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
