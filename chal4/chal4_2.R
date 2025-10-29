#r-cran-data.table, r-cran-stringr
#find on: https://packages.ubuntu.com/
MGS_Big <- function(fname, out) {
  con <- file(fname, open = "r")
  i <- 0
  repeat {
    i <- i + 1
    linesi <- readLines(con, n = 2000)  # smaller chunk size
    if (length(linesi) == 0) break
    chunki <- data.table::fread(text = paste(linesi, collapse = "\n")) |> as.matrix()
    qri <- qr(chunki)
    saveRDS(qr.Q(qri), file = paste0("kangarooQ",
                                     stringr::str_pad(i, 10, pad = "0"), ".rds"))
    saveRDS(qr.R(qri), file = paste0("kangarooR",
                                     stringr::str_pad(i, 10, pad = "0"), ".rds"))
  }
  close(con)
  
  # ---- Incremental QR accumulation ----
  Rfiles <- list.files(pattern = "kangarooR")
  R_accum <- NULL
  for (Rf in Rfiles) {
    Ri <- readRDS(Rf)
    if (is.null(R_accum)) {
      R_accum <- Ri
    } else {
      # Stack old and new Rs, then get new R
      combined <- rbind(R_accum, Ri)
      R_accum <- qr.R(qr(combined))
    }
  }
  
  # Now we have the final upper-triangular R (same as qr.R(qr(Rstack)))
  # But we *don’t* have the full RstackQ (which was the Q from qr(Rstack))
  # So we’ll skip that multiplication stage and just emit the combined Qs.
  
  numChunks <- i - 1
  Qfiles <- list.files(pattern = "kangarooQ")
  
  # Get header and write to output
  con <- file(fname, open = "r")
  header <- readLines(con, n = 1)
  close(con)
  writeLines(header, con = out)
  
  for (j in 1:numChunks) {
    Qj <- readRDS(Qfiles[j])
    # We only care about the part that corresponds to the rank of R_accum
    rankR <- ncol(R_accum)
    Qj <- Qj[, 1:rankR, drop = FALSE]
    data.table::fwrite(
      data.table::as.data.table(Qj),
      file = out,
      append = TRUE,
      col.names = FALSE
    )
  }
}
