#r-cran-data.table
#this can probably be improved by increasing step size
CSV_Col_Means <- function(fname, stepSize = 50009) {
  stepSize <- stepSize
  sumHolder <- 0
  nHolder <- 0
  i <- 0
  err <- FALSE
  repeat {
    #count interation
    i <- i + 1
    
    #read in data of the specific step size skipping previously read lines
    #this is set up in a tryCatch bc if the total number of lines is exactly a 
    #multiple of the step size, this will cause an error. this is very unlikely
    #but it is a possibility. If this does happen, the loop breaks as that means
    #that we have read in all the data
    tryCatch({
      dati <- data.table::fread(fname,
                                skip = (i-1)*stepSize+1,
                                nrow = stepSize)
    },
    error = function(e) {
      err <<- TRUE
    })
    
    #break if there was an error
    if (err) break
    
    #update running sum and running n
    #the issue with this is perceision as it gets large
    sumHolder <- sumHolder + apply(dati, 2, sum)
    nHolder <- nHolder + nrow(dati)
    
    #if the number of rows in dati is not equal to the set step size, then we
    #have read in the very tail of the document and it is time to break
    if (nrow(dati) != stepSize) {
      break
    }
  }
  #obtain the column averages
  return(sumHolder/nHolder)
}




source("H:/schoolFiles/bios7330AdvComputing/challenges(bios7330)/chal2/chal2.R")
setwd("H:/schoolFiles/bios7330AdvComputing/challenges(bios7330)/chal1")
CSV_Col_Means("testDat.csv")
a <- read.csv("testDat.csv")
colMeans(a)
min(CSV_Col_Means("testDat.csv") - colMeans(a)) < .0001
b <- read.csv("testLargeDat.csv")
min(CSV_Col_Means("testLargeDat.csv") - colMeans(b)) < .0001
