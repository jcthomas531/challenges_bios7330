#r-cran-data.table, r-cran-stringr
#find on: https://packages.ubuntu.com/
MGS_Big <- function(fname, out) {
  # STEP 1 — Read data in chunks and save QR results
  con <- file(fname, open = "r")
  i <- 0
  repeat {
    i <- i + 1
    linesi <- readLines(con, n = 20000)  # tune chunk size (e.g., 10000–50000)
    if (length(linesi) == 0) break
    
    chunki <- data.table::fread(text = paste(linesi, collapse = "\n")) |>
      as.matrix()
    
    qri <- qr(chunki)
    saveRDS(qr.Q(qri),
            file = sprintf("kangarooQ%010d.rds", i))
    saveRDS(qr.R(qri),
            file = sprintf("kangarooR%010d.rds", i))
  }
  close(con)
  
  numChunks <- i - 1
  if (numChunks == 0) stop("No chunks read — check input file.")
  
  # STEP 2 — Stack all R matrices
  Rfiles <- list.files(pattern = "kangarooR")
  Rlist  <- lapply(Rfiles, readRDS)
  Rstack <- do.call(rbind, Rlist)
  
  # STEP 3 — QR on stacked R
  RstackQR <- qr(Rstack)
  RstackQ  <- qr.Q(RstackQR, complete = TRUE)
  rankR    <- RstackQR$rank
  
  stride <- floor(nrow(RstackQ) / numChunks)
  
  # STEP 4 — Prepare output
  con <- file(fname, open = "r")
  header <- readLines(con, n = 1)
  close(con)
  writeLines(header, con = out)
  
  Qfiles <- list.files(pattern = "kangarooQ")
  
  # STEP 5 — Blockwise multiplication to control memory
  blockSize <- 500  # number of columns to multiply at once
  
  for (j in 1:numChunks) {
    start <- (j - 1) * stride + 1
    end   <- min(start + stride - 1, nrow(RstackQ))
    RstackQj <- RstackQ[start:end, 1:rankR, drop = FALSE]
    Qj <- readRDS(Qfiles[j])[, 1:rankR, drop = FALSE]
    
    if (ncol(Qj) != nrow(RstackQj)) {
      stop(sprintf("Dimension mismatch at chunk %d: Qj has %d cols, RstackQj has %d rows",
                   j, ncol(Qj), nrow(RstackQj)))
    }
    
    # Multiply in column blocks to avoid huge allocations
    for (k in seq(1, ncol(RstackQj), by = blockSize)) {
      endk <- min(k + blockSize - 1, ncol(RstackQj))
      partial <- Qj %*% RstackQj[, k:endk, drop = FALSE]
      
      data.table::fwrite(
        data.table::as.data.table(partial),
        file = out,
        append = TRUE,
        col.names = FALSE
      )
      rm(partial); gc()
    }
  }
}
