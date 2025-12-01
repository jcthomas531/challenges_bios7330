#r-cran-data.table, r-cran-stringr
MGS_Big <- function(fname, out) {
  library(data.table)
  
  chunk_lines = 20000
  blockSize = 500
  
  # Step 0 — Open file and prepare
  con <- file(fname, open = "r")
  header <- readLines(con, n = 1)
  writeLines(header, con = out)   # write header
  i <- 0
  R_accum <- NULL  # will hold incremental R
  
  # Step 1 — Process each chunk
  repeat {
    linesi <- readLines(con, n = chunk_lines)
    if (length(linesi) == 0) break
    i <- i + 1
    
    chunk <- data.table::fread(text = paste(linesi, collapse = "\n")) |> as.matrix()
    qri <- qr(chunk)
    
    Qj <- qr.Q(qri)  # thin Q
    Ri <- qr.R(qri)
    
    # Step 2 — Incrementally update R_accum
    if (is.null(R_accum)) {
      R_accum <- Ri
    } else {
      combined <- rbind(R_accum, Ri)
      R_accum <- qr.R(qr(combined))
    }
    
    # Step 3 — Transform Qj with current R_accum if needed
    # In the streaming approach, we can skip multiplying by RstackQ, just output Qj blocks
    # Optionally: truncate to rank of R_accum
    rankR <- ncol(R_accum)
    Qj <- Qj[, 1:rankR, drop = FALSE]
    
    # Step 4 — Blockwise write to CSV
    for (k in seq(1, ncol(Qj), by = blockSize)) {
      endk <- min(k + blockSize - 1, ncol(Qj))
      partial <- Qj[, k:endk, drop = FALSE]
      data.table::fwrite(
        data.table::as.data.table(partial),
        file = out,
        append = TRUE,
        col.names = FALSE
      )
      rm(partial); gc()
    }
    
    rm(Qj, Ri, chunk); gc()
  }
  
  close(con)
}
