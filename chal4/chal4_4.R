#r-cran-data.table, r-cran-stringr
MGS_Big <- function(fname, out) {
  library(data.table)
  chunk_lines = 20000
  blockSize = 500
  # Step 0 — Open file and write header
  con <- file(fname, open = "r")
  header <- readLines(con, n = 1)
  writeLines(header, con = out)
  
  # Step 1 — Initialize variables
  i <- 0
  R_accum <- NULL     # incremental R
  Q_accum_list <- list()  # store Qj chunks for later streaming
  chunk_rows <- c()   # track number of rows in each chunk
  
  # Step 2 — Read chunks, compute QR, update R incrementally
  repeat {
    linesi <- readLines(con, n = chunk_lines)
    if (length(linesi) == 0) break
    i <- i + 1
    
    chunk <- data.table::fread(text = paste(linesi, collapse = "\n")) |> as.matrix()
    chunk_rows[i] <- nrow(chunk)
    
    qri <- qr(chunk)
    Qj <- qr.Q(qri, complete = FALSE)
    Ri <- qr.R(qri)
    
    # Incremental QR update: R_accum <- qr.R(rbind(R_accum, Ri))
    if (is.null(R_accum)) {
      R_accum <- Ri
    } else {
      combined <- rbind(R_accum, Ri)
      R_accum <- qr.R(qr(combined))
    }
    
    Q_accum_list[[i]] <- Qj
    rm(Qj, Ri, chunk); gc()
  }
  close(con)
  
  rankR <- ncol(R_accum)  # rank of final R
  
  # Step 3 — Apply the final transformation to each Qj and write to CSV
  for (j in seq_along(Q_accum_list)) {
    Qj <- Q_accum_list[[j]]
    # Truncate to rank
    Qj <- Qj[, 1:rankR, drop = FALSE]
    
    # Corresponding block of R_accum
    start_row <- sum(chunk_rows[1:(j-1)]) + 1
    end_row   <- start_row + chunk_rows[j] - 1
    Rblock <- R_accum[start_row:end_row, 1:rankR, drop = FALSE]
    
    # Memory-safe multiplication in blocks
    for (k in seq(1, ncol(Rblock), by = blockSize)) {
      endk <- min(k + blockSize - 1, ncol(Rblock))
      partial <- Qj %*% Rblock[, k:endk, drop = FALSE]
      
      data.table::fwrite(
        data.table::as.data.table(partial),
        file = out,
        append = TRUE,
        col.names = FALSE
      )
      rm(partial); gc()
    }
    rm(Qj, Rblock); gc()
  }
}
