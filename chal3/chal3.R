MGS <- function(x) {
  qr.Q(qr(x, LAPACK = TRUE))
}
  


