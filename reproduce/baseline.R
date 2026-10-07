# Download pinned generating-model files for new simulations; no MFCL execution.
script <- grep("^--file=", commandArgs(), value = TRUE)
stopifnot(length(script) == 1L)
source(file.path(dirname(normalizePath(sub("^--file=", "", script))), "verify.R"))
root <- reader_root()
args <- commandArgs(trailingOnly = TRUE)
assert(length(args) %in% 1:2 && args[[1L]] %in% c("--list", "--prepare", "--verify"), "Use make baseline-list or prepare INPUT=/absolute/new-folder.")
pins <- data.frame(
  path = c("bet.frq", "bet.ini", "bet.tag", "bet.age_length", "bet.reg_scaling", "mfcl.cfg", "doitall.sh", "final.par"),
  bytes = c(2932052, 66790, 110051, 1403841, 1186, 31, 48341, 2956541),
  sha256 = c("d0d84f0a498e6a62681f2a58ffc1ba53dab9e3d6af856b4ad1fd907196250004", "5292938d4743c1dfdd2f1a095c1aa87482c9c17f78b8d879671fe6851d58646f", "b140e66eb52f2b7e022ef2c562134f8bc9baf3dede18ce95283a001acd2b013f", "426859b825bd815aa69c8d97c9dd93097027ed1eb6b9e444d88b69562097a00c", "5f047ddb4053d1f6df9ace18e85e440b11553de246d024ce8138b427f5f9f7e3", "2ec8a291fae62c6f37541aec1de37444626d42b3290b371bb42b63d510034eae", "ad8ca660b6d84f9bbd1d8024f616a5bd66047a44ecc2c6e8b5c61c6be089fc5c", "21dcaea9db8c89ddc8c29fa3c3a5e514b50bef6e26587c168c00c05f35fbebc3"),
  stringsAsFactors = FALSE
)
pins <- rbind(pins, data.frame(path = "mfclo64", bytes = 34549392, sha256 = "f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0"))
jitter <- "https://raw.githubusercontent.com/PacificCommunity/ofp-sam-bet-2026-jitter/bb3f4016b2d145f42c7a76072ed2b10b49aff71f/"
pins$url <- paste0(jitter, "data/diagnostic/mfcl/", pins$path)
pins$url[pins$path == "final.par"] <- paste0(jitter, "data/diagnostic/reproduction/fitted-reference/final.par")
pins$url[pins$path == "bet.ini"] <- "https://raw.githubusercontent.com/PacificCommunity/ofp-sam-bet-2026-diagnostic/3abf0c64fb9b0c2d70b9c672dc7d9a655d3060d6/model/bet.ini"
pins$mode <- ifelse(pins$path %in% c("doitall.sh", "mfclo64"), 493, 420)
if (args[[1L]] == "--list") {
  assert(length(args) == 1L, "List accepts no output.")
  utils::write.table(pins, stdout(), sep = "\t", row.names = FALSE, quote = FALSE)
} else {
  assert(length(args) == 2L, "Expected a baseline INPUT path.")
  output <- args[[2L]]
  if (args[[1L]] == "--prepare") {
    fresh_output(output, root)
    assert(dir.create(output, mode = "0700", showWarnings = FALSE), "Could not create fresh INPUT.")
    old_timeout <- getOption("timeout")
    options(timeout = max(120, old_timeout))
    for (i in seq_len(nrow(pins))) {
      target <- file.path(output, pins$path[[i]])
      no_links(target); assert(!file.exists(target) && !dir.exists(target), "Existing baseline member refused.")
      downloaded <- file.path(normalizePath(tempdir(), mustWork = TRUE), basename(tempfile("bet-download-")))
      status <- utils::download.file(pins$url[[i]], downloaded, method = "libcurl", mode = "wb", quiet = TRUE)
      assert(status == 0L, "Pinned source download failed: ", pins$path[[i]])
      checked_file(downloaded, as.list(pins[i, ]))
      write_new(readBin(downloaded, "raw", pins$bytes[[i]]), target)
      unlink(downloaded)
      checked_file(target, as.list(pins[i, ]))
      assert(Sys.chmod(target, sprintf("%04o", as.integer(pins$mode[[i]]))), "Could not set baseline permissions.")
    }
    options(timeout = old_timeout)
  }
  for (i in seq_len(nrow(pins))) checked_file(file.path(output, pins$path[[i]]), as.list(pins[i, ]), executable = pins$mode[[i]] == 493)
  cat("Verified generating-model baseline and MFCL engine in", output, "; original 50 replicate files remain unavailable.\n")
}
