serialize_ <- function(x) {
  # WARNING: Use bzip2 in order to increase performance but decrease
  #   compression ratio
  paste(memCompress(serialize(x, NULL, ascii = F, xdr = T), type = c("xz")),
    collapse = "~"
  )
}

unserialize_ <- function(x) {
  if (is.na(x)) {
    return(NA)
  }
  unserialized <- tryCatch(
    {
      unserialize(memDecompress(
        as.raw(as.hexmode(unlist(strsplit(as.character(x), "~"))))
      ))
    },
    error = function(e) {
      NA
    }
  )
  return(unserialized)
}

save <- function(x, filename, serialize = FALSE) {
  if (serialize == FALSE) {
    saveRDS(x, file = filename)
  } else {
    write.csv(serialize_(x), file = filename, row.names = FALSE)
  }
}

load <- function(filename, serialize = FALSE) {
  if (serialize == FALSE) {
    readRDS(file = filename)
  } else {
    content <- read.csv(file = filename)
    unserialize_(content$x)
  }
}
