#helpers
#project vector x onto vector y
#proj_y(x)
vecProj <- function(y,x) {
  as.numeric(crossprod(x,y)/crossprod(y))*y
}
#L2 norm
normL2 <- function(x) {
  as.numeric(sqrt(crossprod(x)))
}
#main function
MGS <- function(X){
  #define orthonormal matrix
  U <- matrix(nrow = nrow(X), ncol = ncol(X))
  #first vector is just the first X vector, normalized at end
  U[,1] <- X[,1]
  #loop thru other columns
  for (i in 2:ncol(X)) {
    #the vector i depends on all previous vectors up until i
    #uSuper keeps track of the iteration process
    uSuper <- matrix(nrow = nrow(X), ncol = i)
    #the first vector in uSuper is the vector we are working with
    uSuper[,1] <- X[,i]
    #iterate thru the previously calculated U columns
    for (j in 2:ncol(uSuper)) {
      uSuper[,j] <- uSuper[,(j-1)] - vecProj(y = U[,(j-1)], x = uSuper[,(j-1)])
    }
    U[,i] <- uSuper[,ncol(uSuper)]
  }
  #normalize
  U <- apply(U, 2, function(x) x/normL2(x))
  return(U)
}