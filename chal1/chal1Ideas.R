################################################################################
#testing means of means
x <- 1:10
y <- 6:10
z <- 11:15
xbar <- mean(x)
ybar <- mean(y)
zbar <- mean(z)
xyzbar <- mean(c(xbar,ybar, zbar))
fullbar <- mean(c(x,y,z))
xyzbar
fullbar
################################################################################
#plan
#taking means as we go along
#read in lines at a time
#will need some sort of stopping procedure

################################################################################
#making data to play with
setwd("H:/schoolFiles/bios7330AdvComputing/challenges(bios7330)/chal1")
set.seed(826)
n <- 100
# dat <- matrix(c(rnorm(n, 1, 1),
#                 rnorm(n, 2, 1),
#                 rnorm(n, 3, 1),
#                 rnorm(n, 4, 1),
#                 rnorm(n, 5, 1)), ncol = 5)
# write.csv(dat, "testDat.csv", row.names = FALSE, col.names = FALSE)
dat <- data.table::fread("testDat.csv")
apply(dat, 2, mean)



n2 <- 1e6
# dat2 <- matrix(c(rnorm(n2, 1, 1),
#                  rnorm(n2, 2, 1),
#                  rnorm(n2, 3, 1),
#                  rnorm(n2, 4, 1),
#                  rnorm(n2, 5, 1)), ncol = 5)
# 
# write.csv(dat2, "testLargeDat.csv", row.names = FALSE, col.names = FALSE)
dat2 <- data.table::fread("testLargeDat.csv")
apply(dat2, 2, mean)
################################################################################
#this seems to be working
stepSize <- 9
sumHolder <- 0
nHolder <- 0
i <- 0
err <- FALSE
repeat {
  i <- i + 1
  tryCatch({
    dati <- data.table::fread("testDat.csv",
                              skip = (i-1)*stepSize+1,
                              nrow = stepSize)
  },
  error = function(e) {
    #this should really only come in to play if it so happens that the length
    #of the csv is an exact multiple of the step size
    #must activate this in parent environment
    err <<- TRUE
  })

  # dati <- data.table::fread("testDat.csv",
  #                           skip = (i-1)*stepSize+1,
  #                           nrow = stepSize)
  if (err) break
  #the issue with this is perceision as it gets large
  sumHolder <- sumHolder + apply(dati, 2, sum)
  nHolder <- nHolder + nrow(dati)
  if (nrow(dati) != stepSize) {
    break
  }
  
  print(i)
}
sumHolder/nHolder

################################################################################


#functionized:
CSV_Col_Means <- function(fname) {
  stepSize <- 9999
  sumHolder <- 0
  nHolder <- 0
  i <- 0
  err <- FALSE
  repeat {
    i <- i + 1
    print(i)
    tryCatch({
      dati <- data.table::fread(fname,
                                skip = (i-1)*stepSize+1,
                                nrow = stepSize)
    },
    error = function(e) {
      #this should really only come in to play if it so happens that the length
      #of the csv is an exact multiple of the step size
      #must activate this in parent environment
      err <<- TRUE
    })
    
    # dati <- data.table::fread("testDat.csv",
    #                           skip = (i-1)*stepSize+1,
    #                           nrow = stepSize)
    if (err) break
    #the issue with this is perceision as it gets large
    sumHolder <- sumHolder + apply(dati, 2, sum)
    nHolder <- nHolder + nrow(dati)
    if (nrow(dati) != stepSize) {
      break
    }
    
    
  }
  sumHolder/nHolder
}
CSV_Col_Means("testDat.csv")
apply(dat, 2, mean)
CSV_Col_Means("testLargeDat.csv")
apply(dat2, 2, mean)
#i think this is prety good
################################################################################


#potnential upgrades:
#setting sep to ","
#setting colClasses to numeric
#setting dec to "."
#setting encoding
#showProgress to false
#nthreads? is there parallel options on the machine?

#skip will help us get where we need to in the file
fullDat <- data.table::fread("testDat.csv")
fullMean <- apply(fullDat, 2, mean)



#testing line by line
dat1 <- data.table::fread("testDat.csv", skip = 0, nrow = 100)
dat2 <- data.table::fread("testDat.csv", skip = 101)
tail(dat1)
head(dat2)

dat1[nrow(dat1), ] == dat2[1,]



dat1Sum <- apply(dat1, 2, sum)
dat2Sum <- apply(dat2, 2, sum)
(dat1Sum[1] + dat2Sum[1])/10000
#this works, it gets us accuracy to the 15th decimal place now
#but will this get worse at larger size?


#what happens when we tell it to read more than it has
datToFar <- data.table::fread("testDat.csv", skip = 500, nrow = 10000)
tail(datToFar)
nrow(datToFar)
#it will just stop when it gets to the end
#we can check this behavior by making sure that the amount of rows in the read
#in data are the same as the amount of rows we are asking for, if it is not, then we stop
#at most, we will do one too many iterations




#now lets try it with extremely large data



#
fullLargeDat <- data.table::fread("testLargeDat.csv")
fullLargeMean <- apply(fullLargeDat, 2, mean)



#testing line by line
stepSize <- n2/2
largeDat1 <- data.table::fread("testLargeDat.csv", skip = 1, nrow = stepSize)
largeDat2 <- data.table::fread("testLargeDat.csv", skip = 1*stepSize+1, nrow = stepSize)
largeDat3 <- data.table::fread("testLargeDat.csv", skip = 2*stepSize+1, nrow = stepSize)
largeDat4 <- data.table::fread("testLargeDat.csv", skip = 3*stepSize+1, nrow = stepSize)
largeDat1Sum <- apply(largeDat1, 2, sum)
largeDat2Sum <- apply(largeDat2, 2, sum)
(largeDat1Sum[1] + largeDat2Sum[1])/n2
#this gets accuracy out to about 12 decimal points

#could get a running sum and a running n and divide at end
#the only issue here is losing accuracy
#if i keep a running sum then i am more likely to get into big numbers
#but if i keep track of each sum then i am using more memory
#could i do the log and then exponentiate???

stepSize = 9999
#perhaps there is a better way to initialize the while loop
dati <- data.frame(rep(NA, stepSize))
i <- 0
while (nrow(dati) == stepSize) {
  i <- i + 1
  dati <- data.table::fread("testLargeDat.csv",
                            skip = (i-1)*stepSize+1,
                            nrow = stepSize)
  print(i)
  
  
  
}
n2/9999


sumHolder <- 0
nHolder <- 0
repeat {
  tryCatch({
    dati <- data.table::fread("testLargeDat.csv",
                              skip = (i-1)*stepSize+1,
                              nrow = stepSize)
  },
  error = function(e) {
    #this should really only come in to play if it so happens that the length
    #of the csv is an exact multiple of the step size
    break
  })
  sumHolder <- sumHolder + apply(dati, 2, sum)
  nHolder <- nHolder + nrow(dati)
  
}



#tryCatch the error


apply(tryCatch({
  data.table::fread("testLargeDat.csv", 
                    skip = n2/2, nrow = stepSize-200)
}, 
error = function(e){
  #this should really only come in to play if it so happens that the length
  #of the csv is an exact multiple of the step size
  data.frame(NA)
}), 2, sum, na.rm = TRUE)




tryCatch({
  dati <- data.table::fread("testLargeDat.csv", 
                    skip = 0, nrow = stepSize-38)
}, 
error = function(e){
  #this should really only come in to play if it so happens that the length
  #of the csv is an exact multiple of the step size
  message("your mom")
})






#i need to understand that skip and nrow relationship
dat3 <- data.frame(1:50)
write.csv(dat3, "testNumberDat.csv")
numDat1 <- data.table::fread("testNumberDat.csv", skip = 1, nrow = 5)
numDat1
numDat2 <- data.table::fread("testNumberDat.csv", skip = 5+1, nrow = 5)
numDat2
numDat3 <- data.table::fread("testNumberDat.csv", skip = 10+1, nrow = 5)
numDat3








