


#quick simulation to show that breaking up the RstackQ, multiplying it with
#the respective Q_i, and then stacking the results gives you the same
#things as the block matrix multiplication
# set.seed(826)
# A1 <- matrix(sample(1:10, 24, replace = TRUE), nrow = 8, ncol = 3)
# A2 <- matrix(sample(1:10, 24, replace = TRUE), nrow = 8, ncol = 3)
# A <- Matrix::bdiag(A1, A2)
# B <- matrix(sample(1:10, 18, replace = TRUE), nrow = 6, ncol = 3)
# B1 <- B[1:3,]
# B2 <- B[4:6,]
# fullRes <- (A %*% B) |> as.matrix()
# stackRes <- rbind(A1 %*% B1, A2 %*% B2)
# dim(fullRes)
# dim(stackRes)
# all.equal(fullRes, stackRes)





# #Q when the data is not split up
# trueQ <- qr.Q(qr(dat))
# #split the data up
# dat1 <- dat[1:30,]
# dat2 <- dat[31:10000,]
# #first part of data
# Q1 <- qr.Q(qr(dat1))
# R1 <- qr.R(qr(dat1))
# #second part of data
# Q2 <- qr.Q(qr(dat2))
# R2 <- qr.R(qr(dat2))
# #stack R matricies
# Rstack <- rbind(R1, R2)
# #QR decomp on the R stack
# RstackQ <- qr.Q(qr(Rstack))
# RstackR <- qr.R(qr(Rstack))
# #signs, dont think this is necessary given what he said in instructions but
# #if it is, just replace RstackQ with RstackQpos in newQ assignment
# # s <- sign(diag(RstackR))
# # s[s==0] <- 1
# # S <- diag(s)
# # RstackQpos <- RstackQ %*% S
# #obtain Q
# newQ <- Matrix::bdiag(Q1, Q2) %*% RstackQ
# 
# 
# 
# #columns may be scaled by 1 and neg 1 differently, make everything positive 
# #so that we can check the numbers
# newQPos <- apply(as.matrix(newQ), 2, abs)
# trueQpPos <- apply(trueQ, 2, abs)
# all.equal(newQPos, trueQpPos)







library(data.table)
library(stringr) #if you cant get stringr on the machine then there are work arounds, this is just convienent

#IN ORDER FOR THIS TO WORK PROPERLY, THE DIRECTORY MUST HAVE NO .RDS FILES
#generate some made up data
nr <- sample(10000:50000, 1)
nc <- sample(25:50, 1)
csize <- sample(1000:6000,1)
set.seed(826)
dat <- matrix(data = rnorm(nr*nc), nrow = nr, ncol = nc)
setwd("H:/schoolFiles/bios7330AdvComputing/challenges(bios7330)/chal4")
write.csv(dat, "exampleData.csv", row.names = FALSE)






#so now the question is can we deploy something like this in a memory efficient manner
#i dont know how large the data set is






#lets run thru this and try saving R matrices
#this is what the read in process should look like


#step 1 read in all chuncks and output Q and R matrices
#readLines can read unitl failure and whener we get a length of 0 then break
#outputtting the matrices as RDS is space efficient
#if this doesnt cut it, then raw binary files may make space
con <- file("exampleData.csv", open = "r")

i <- 0
repeat {
  i <- i + 1
  linesi <- readLines(con, n=csize)
  if (length(linesi) == 0) {break}
  chunki <- fread(paste(linesi, collapse = "\n")) |>
    as.matrix()
  qri <- qr(chunki)
  #these will need to be read in later by name so giving unique name 
  #i doubt grant will have anything named "kangaroo" in his directory
  #need to add leading zeros so that these will read in in the right order
  saveRDS(qr.Q(qri), file = paste0("kangarooQ",
                                   stringr::str_pad(i, 10, pad = "0"),
                                   ".rds"))
  saveRDS(qr.R(qri), file = paste0("kangarooR",
                                   stringr::str_pad(i, 10, pad = "0"),
                                   ".rds"))
}
close(con)


#step 2, bring in the Rs and stack them....order is going to be important here...
#the naming should take care of this
#to do this, need to get all files named kangarooR

Rfiles <- list.files()[grep("kangarooR", list.files())]
Rlist <- lapply(Rfiles, readRDS)
Rstack <- do.call(rbind, Rlist)


#step 3 do the qr decomp on the R stack, only care about the Q
RstackQ <- qr.Q(qr(Rstack))


#step 4, bring in each Q individually and mutliply it by the RstackQ and output it
#into a new csv, stacking each time

#i think this stride is ineligant bc that value is going to be the number of cols
#in the original matrix but it seems to be working...
numChunks <- i - 1
stride <- ncol(RstackQ) #nrow(RstackQ)/numChunks
#this should preserve order
Qfiles <- list.files()[grep("kangarooQ", list.files())]
testList <- vector(mode = "list", length = numChunks)

#make header for the csv
con <- file("exampleData.csv", open = "r")
header <- readLines(con, n=1)
close(con)
writeLines(header, con = "finalQ.csv")
for (j in 1:numChunks) {
  #get the chunk of the RstackQ that we need
  indOffset <- (j-1)*stride
  RstackQj <- RstackQ[(indOffset+1):(indOffset+stride),]
  #read in the corresponding Qj
  Qj <- readRDS(Qfiles[j])
  #testing output
  testList[[j]] <- Qj %*% RstackQj
  #writing
  fwrite(as.data.table(Qj %*% RstackQj),
         file = "finalQ.csv",
         append = TRUE,
         col.names = FALSE)
}

#gonna output them as a list for this first go around but then write them out as they come
Qhard <- do.call(rbind, testList)
QhardPos <- apply(Qhard, 2, abs)

datEasy <- read.csv("exampleData.csv")
Qeasy <- qr.Q(qr(datEasy))
QeasyPos <- apply(Qeasy, 2, abs)


Qfinal <- read.csv("finalQ.csv") |>
  as.matrix()
QfinalPos <- apply(Qfinal, 2, abs) 
dimnames(QfinalPos) <- NULL

attributes(QhardPos)
attributes(QeasyPos)
attributes(QfinalPos)



dim(Qhard) == dim(Qeasy)
dim(Qhard) == dim(Qfinal)
dim(Qeasy) == dim(Qfinal)
all.equal(QhardPos, QeasyPos)
all.equal(QhardPos, QfinalPos)
all.equal(QeasyPos, QfinalPos)
nr
nc
csize












################################################################################
#testing function
nr <- sample(300000, 1)
nc <- sample(25:50, 1)
set.seed(826)
dat <- matrix(data = rnorm(nr*nc), nrow = nr, ncol = nc)
setwd("H:/schoolFiles/bios7330AdvComputing/challenges(bios7330)/chal4")
write.csv(dat, "exampleData.csv", row.names = FALSE)
source("H:/schoolFiles/bios7330AdvComputing/challenges(bios7330)/chal4/chal4.R")
MGS_Big(fname = "exampleData.csv", out = "finalQ.csv")


datEasy <- read.csv("exampleData.csv")
Qeasy <- qr.Q(qr(datEasy))
QeasyPos <- apply(Qeasy, 2, abs)


Qfinal <- read.csv("finalQ.csv") |>
  as.matrix()
QfinalPos <- apply(Qfinal, 2, abs) 
dimnames(QfinalPos) <- NULL

dim(Qeasy) == dim(Qfinal)
all.equal(QeasyPos, QfinalPos)
nr
nc


