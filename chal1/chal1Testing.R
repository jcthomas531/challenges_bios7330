setwd("H:/schoolFiles/bios7330AdvComputing/challenges(bios7330)/chal1")
source("chal1.R")
lDat <- data.table::fread("testLargeDat.csv")
apply(lDat, 2, mean)
CSV_Col_Means("testLargeDat.csv")


t1 <- system.time(CSV_Col_Means("testLargeDat.csv", stepSize = 9999))
t2 <- system.time(CSV_Col_Means("testLargeDat.csv", stepSize = 59999))