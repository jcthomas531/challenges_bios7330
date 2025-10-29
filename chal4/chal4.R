#library(data.table)
#library(stringr) #if you cant get stringr on the machine then there are work arounds, this is just convienent



#r-cran-data.table, r-cran-stringr
#find on: https://packages.ubuntu.com/
MGS_Big <- function(fname, out) {
  #step 1 read in all chuncks and output Q and R matrices
  #readLines can read unitl failure and whener we get a length of 0 then break
  #outputtting the matrices as RDS is space efficient
  #if this doesnt cut it, then raw binary files may make space
  con <- file(fname, open = "r")
  
  i <- 0
  repeat {
    i <- i + 1
    linesi <- readLines(con, n=10000)
    if (length(linesi) == 0) {break}
    chunki <- data.table::fread(text = paste(linesi, collapse = "\n")) |>
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
  RstackQR <- qr(Rstack)
  RstackQ  <- qr.Q(RstackQR, complete = TRUE)
  rankR    <- RstackQR$rank


  #step 4, bring in each Q individually and mutliply it by the RstackQ and output it
  #into a new csv, stacking each time

  #i think this stride is ineligant bc that value is going to be the number of cols
  #in the original matrix but it seems to be working...
  numChunks <- i - 1
  #stride <- ncol(RstackQ) #nrow(RstackQ)/numChunks
  stride <- floor(nrow(RstackQ) / numChunks)
  
  
  #this should preserve order
  Qfiles <- list.files()[grep("kangarooQ", list.files())]
  

  #make header for the csv
  con <- file(fname, open = "r")
  header <- readLines(con, n=1)
  close(con)
  writeLines(header, con = out)
  for (j in 1:numChunks) {
    start <- (j - 1) * stride + 1
    end   <- min(start + stride - 1, nrow(RstackQ))
    RstackQj <- RstackQ[start:end, 1:rankR, drop = FALSE]
    Qj <- readRDS(Qfiles[j])[, 1:rankR, drop = FALSE]
    
    if (ncol(Qj) != nrow(RstackQj)) {
      stop(sprintf("Still mismatch at chunk %d: Qj has %d cols, RstackQj has %d rows",
                   j, ncol(Qj), nrow(RstackQj)))
    }
    
    data.table::fwrite(
      data.table::as.data.table(Qj %*% RstackQj),
      file = out,
      append = TRUE,
      col.names = FALSE)
  }

  
}





