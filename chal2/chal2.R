#r-cran-data.table
CSV_Col_Means <- function(fname, stepSize = 350009) {
  stepSize <- stepSize
  sumHolder <- 0
  nHolder <- 0
  con <- file(fname, open = "r")
  i <- 0
  repeat {
    #count interation
    i <- i + 1
    linesi <- readLines(con, n=stepSize)
    if (length(linesi) == 0) {break}
    chunki <- data.table::fread(text = paste(linesi, collapse = "\n")) |>
      as.matrix()
    
    
    #update running sum and running n
    #the issue with this is perceision as it gets large
    sumHolder <- sumHolder + apply(chunki, 2, sum)
    nHolder <- nHolder + nrow(chunki)
  }
  #obtain the column averages
  return(sumHolder/nHolder)
}


