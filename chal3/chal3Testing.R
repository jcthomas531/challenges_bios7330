#helpers
#project vector x onto vector y
#proj_y(x)
vecProj <- function(y,x) {
  x <- matrix(x, ncol = 1)
  y <- matrix(y, ncol = 1)
  c((t(x)%*%y)/(t(y)%*%y)) * y
}
vecProj2 <- function(y,x) {
  as.numeric(crossprod(x,y)/crossprod(y))*y
}
x <- rnorm(1e6)
y <- rnorm(1e6)
system.time(for (i in 1:1000) Rfast::Norm(matrix(x, ncol=1), type="F"))
system.time(for (i in 1:1000) normL2_2(x))

#L2 norm
normL2 <- function(x) {
  x <- matrix(x, ncol = 1)
  c(sqrt(t(x)%*%x))
}
normL2_2 <- function(x) {
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
      uSuper[,j] <- uSuper[,(j-1)] - vecProj2(y = U[,(j-1)], x = uSuper[,(j-1)])
    }
    U[,i] <- uSuper[,ncol(uSuper)]
  }
  #normalize
  U <- apply(U, 2, function(x) x/normL2_2(x))
  return(U)
}
X <- iris[1:4] |> as.matrix()
tail(MGS(X))
microbenchmark::microbenchmark(MGS(X))

#build in option
qr(X)
tail(qr.Q(qr(X)))
microbenchmark::microbenchmark(qr.Q(qr(X)))

#from rfast
tail(pracma::gramSchmidt(X)$Q)
microbenchmark::microbenchmark(pracma::gramSchmidt(X)$Q)




