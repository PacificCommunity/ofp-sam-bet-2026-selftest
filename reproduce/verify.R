# Base-R readers for pinned files and ordinary tar archives; no model execution.
fail <- function(...) stop(..., call. = FALSE)
assert <- function(ok, ...) if (!isTRUE(ok)) fail(...)

read_json <- function(path) {
  assert(regular(path)$size <= 2 * 1024^2, "Manifest exceeds the reader limit.")
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  at <- 1L; total <- nchar(text, type = "chars")
  take <- function() substr(text, at, at)
  white <- function() {
    while (at <= total && take() %in% c(" ", "\t", "\r", "\n")) at <<- at + 1L
  }
  string <- function() {
    assert(take() == '"', "Invalid JSON string.")
    at <<- at + 1L; out <- character()
    repeat {
      assert(at <= total, "Unterminated JSON string.")
      c <- take(); at <<- at + 1L
      if (c == '"') return(paste0(out, collapse = ""))
      if (c == "\\") {
        assert(at <= total, "Invalid JSON escape.")
        c <- take(); at <<- at + 1L
        escapes <- c('"' = '"', "\\" = "\\", "/" = "/", b = "\b", f = "\f", n = "\n", r = "\r", t = "\t")
        if (c == "u") {
          hex <- substr(text, at, at + 3L)
          assert(grepl("^[0-9A-Fa-f]{4}$", hex), "Invalid JSON Unicode escape.")
          code <- strtoi(hex, 16L); at <<- at + 4L
          assert(code > 0L && !code %in% 0xD800:0xDFFF, "Unsupported JSON Unicode character.")
          c <- intToUtf8(code)
        } else {
          assert(c %in% names(escapes), "Invalid JSON escape.")
          c <- unname(escapes[[c]])
        }
      } else assert(utf8ToInt(c) >= 32L, "Control character in JSON string.")
      out <- c(out, c)
    }
  }
  value <- function() {
    white(); assert(at <= total, "Incomplete JSON.")
    c <- take()
    if (c == '"') return(string())
    if (c %in% c("{", "[")) {
      object <- c == "{"; end <- if (object) "}" else "]"
      at <<- at + 1L; white(); out <- list()
      if (take() == end) { at <<- at + 1L; return(out) }
      repeat {
        white()
        if (object) {
          key <- string(); assert(!key %in% names(out), "Duplicate JSON key.")
          white(); assert(take() == ":", "Missing JSON colon."); at <<- at + 1L
        } else key <- length(out) + 1L
        out[key] <- list(value()); white()
        if (take() == end) { at <<- at + 1L; return(out) }
        assert(take() == ",", "Invalid JSON separator."); at <<- at + 1L
      }
    }
    rest <- substring(text, at)
    for (word in c("true", "false", "null")) {
      if (startsWith(rest, word)) {
        at <<- at + nchar(word)
        return(switch(word, true = TRUE, false = FALSE, null = NULL))
      }
    }
    found <- regexpr("^-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?", rest)
    assert(found[[1L]] == 1L, "Invalid JSON value.")
    token <- regmatches(rest, found); at <<- at + nchar(token)
    number <- as.numeric(token); assert(is.finite(number), "Nonfinite JSON number.")
    number
  }
  out <- value(); white(); assert(at > total, "Trailing JSON content."); out
}

reader_root <- function() {
  script <- grep("^--file=", commandArgs(), value = TRUE)
  assert(length(script) == 1L, "Use Rscript or the Make entry points.")
  dirname(dirname(normalizePath(sub("^--file=", "", script), mustWork = TRUE)))
}
safe_member <- function(path) {
  assert(is.character(path) && length(path) == 1L && nzchar(path) &&
    !startsWith(path, "/") && !grepl("[\\\\\r\n]", path) &&
    !any(strsplit(path, "/", fixed = TRUE)[[1L]] %in% c("", ".", "..")) &&
    !endsWith(path, "/"), "Invalid archived path.")
  path
}
no_links <- function(path) {
  for (node in unique(c(path, dirname(path)))) {
    repeat {
      link <- Sys.readlink(node)
      assert(is.na(link) || !nzchar(link), "Symbolic link in path: ", node)
      parent <- dirname(node); if (identical(node, parent)) break; node <- parent
    }
  }
  invisible(path)
}
regular <- function(path) {
  no_links(path); info <- file.info(path)
  assert(file_test("-f", path) && !is.na(info$size) && !isTRUE(info$isdir), "Expected a regular file: ", path)
  info
}
sha256_file <- function(path) {
  regular(path)
  program <- Sys.which("sha256sum"); args <- shQuote(path)
  if (!nzchar(program)) { program <- Sys.which("shasum"); args <- c("-a", "256", shQuote(path)) }
  assert(nzchar(program), "Install sha256sum (or shasum) to verify saved bytes.")
  lines <- system2(program, args, stdout = TRUE, stderr = TRUE)
  assert(is.null(attr(lines, "status")) && length(lines) == 1L &&
    grepl("^[0-9a-f]{64}[[:space:]]", lines), "SHA256 command failed.")
  substr(lines, 1L, 64L)
}
fresh_output <- function(path, root) {
  assert(is.character(path) && length(path) == 1L && nzchar(path) && startsWith(path, "/") &&
    path != "/" && !grepl("//", path, fixed = TRUE) && !grepl("[\r\n]", path) &&
    !any(strsplit(path, "/", fixed = TRUE)[[1L]] %in% c(".", "..")) &&
    !endsWith(path, "/"), "Choose a clean absolute path to a new folder.")
  no_links(path)
  inside <- function(p, r) identical(p, r) || startsWith(p, paste0(r, "/"))
  assert(!inside(path, root) && !inside(root, path), "Output must be outside the checkout.")
  assert(!file.exists(path) && !dir.exists(path), "Existing output refused.")
  assert(dir.exists(dirname(path)), "Output parent must already exist.")
  path
}
checked_file <- function(path, row, executable = FALSE) {
  assert(is.numeric(row$bytes) && length(row$bytes) == 1L && is.finite(row$bytes) && row$bytes >= 0 && row$bytes == floor(row$bytes) && is.character(row$sha256) && length(row$sha256) == 1L && grepl("^[0-9a-f]{64}$", row$sha256), "Invalid saved-file binding.")
  before <- regular(path)
  assert(before$size == row$bytes && identical(sha256_file(path), row$sha256), "Saved bytes differ: ", path)
  assert(identical(before[, c("size", "mtime", "ctime", "mode")],
                   file.info(path)[, c("size", "mtime", "ctime", "mode")]), "Source changed during verification.")
  if (executable) assert(file.access(path, 1L) == 0L, "Executable permission missing: ", path)
  invisible(TRUE)
}

checked_tar <- function(root, manifest, keep = FALSE) {
  assert(is.numeric(manifest$schema_version) && identical(manifest$schema_version, 1) && length(manifest$files) > 0L &&
    length(manifest$files) <= 1000L, "Unsupported saved archive manifest.")
  rows <- manifest$files
  paths <- vapply(rows, function(r) safe_member(r$path), character(1L))
  assert(!anyDuplicated(paths), "Duplicate manifest member.")
  for (r in rows) assert(is.numeric(r$bytes) && length(r$bytes) == 1L && is.finite(r$bytes) && r$bytes == floor(r$bytes) && r$bytes > 0 &&
    r$bytes <= 40 * 1024^2 && is.numeric(r$mode) && length(r$mode) == 1L && r$mode %in% c(384, 420, 493) &&
    is.character(r$sha256) && length(r$sha256) == 1L && grepl("^[0-9a-f]{64}$", r$sha256), "Invalid member binding.")
  assert(sum(vapply(rows, function(r) r$bytes, numeric(1L))) <= 160 * 1024^2, "Archive exceeds the reader limit.")
  archive <- file.path(root, "reproduce", safe_member(manifest$archive$path))
  checked_file(archive, manifest$archive)
  con <- gzfile(archive, "rb"); on.exit(close(con), add = TRUE)
  scratch <- file.path(normalizePath(tempdir(), mustWork = TRUE), basename(tempfile("bet-member-"))); on.exit(unlink(scratch), add = TRUE)
  field <- function(raw) {
    end <- match(as.raw(0), raw, nomatch = length(raw) + 1L)
    if (end <= length(raw)) assert(all(raw[end:length(raw)] == as.raw(0)), "Malformed tar text field.")
    if (end == 1L) "" else rawToChar(raw[seq_len(end - 1L)])
  }
  octal <- function(raw) {
    text <- trimws(rawToChar(raw[raw != as.raw(0)]))
    assert(grepl("^[0-7]+$", text), "Unsupported tar number.")
    strtoi(text, 8L)
  }
  seen <- character(); contents <- list(); expanded <- 0
  repeat {
    header <- readBin(con, "raw", 512L)
    assert(length(header) == 512L, "Truncated tar header.")
    expanded <- expanded + 512L
    if (all(header == as.raw(0))) {
      tail <- readBin(con, "raw", 512L)
      assert(length(tail) == 512L && all(tail == as.raw(0)), "Missing tar end marker.")
      repeat {
        tail <- readBin(con, "raw", 8192L)
        if (!length(tail)) break
        expanded <- expanded + length(tail)
        assert(expanded < 170 * 1024^2 && all(tail == as.raw(0)), "Unexpected trailing tar content.")
      }
      break
    }
    sum_header <- header; sum_header[149:156] <- charToRaw("        ")
    assert(sum(as.integer(sum_header)) == octal(header[149:156]), "Tar header checksum differs.")
    name <- field(header[1:100]); prefix <- field(header[346:500])
    if (nzchar(prefix)) name <- paste(prefix, name, sep = "/")
    safe_member(name)
    assert(name %in% paths && !name %in% seen &&
      header[[157L]] %in% c(as.raw(0), charToRaw("0")) &&
      !nzchar(field(header[158:257])), "Unexpected, duplicate or nonregular tar member.")
    row <- rows[[match(name, paths)]]
    assert(octal(header[125:136]) == row$bytes && octal(header[101:108]) == row$mode,
      "Tar size or mode differs: ", name)
    body <- readBin(con, "raw", row$bytes)
    assert(length(body) == row$bytes, "Truncated member: ", name)
    pad <- (512L - row$bytes %% 512L) %% 512L
    if (pad) { padding <- readBin(con, "raw", pad); assert(length(padding) == pad && all(padding == as.raw(0)), "Invalid tar padding.") }
    expanded <- expanded + row$bytes + pad
    assert(expanded < 170 * 1024^2, "Expanded tar limit exceeded.")
    writeBin(body, scratch); assert(identical(sha256_file(scratch), row$sha256), "Member checksum differs: ", name)
    seen <- c(seen, name)
    if (keep) contents[[name]] <- body
  }
  assert(length(seen) == length(paths) && setequal(seen, paths), "Archive coverage differs.")
  checked_file(archive, manifest$archive)
  list(rows = rows, files = contents)
}
write_new <- function(body, path) {
  no_links(path)
  assert(!file.exists(path) && !dir.exists(path), "Existing member refused.")
  con <- file(path, open = "wxb")  # Exclusive binary creation: never truncate an existing file.
  on.exit(close(con), add = TRUE)
  writeBin(body, con); flush(con)
  invisible(path)
}
write_members <- function(output, bundle, root) {
  fresh_output(output, root)
  assert(dir.create(output, mode = "0700", showWarnings = FALSE), "Could not create fresh output.")
  # Never remove a partial output on failure: it remains available for inspection.
  expected <- character()
  for (row in bundle$rows) {
    no_links(output); target <- file.path(output, safe_member(row$path))
    parent <- dirname(target)
    if (!dir.exists(parent)) assert(dir.create(parent, recursive = TRUE, mode = "0700", showWarnings = FALSE), "Could not create member directory.")
    no_links(target); assert(!file.exists(target) && !dir.exists(target), "Existing member refused.")
    write_new(bundle$files[[row$path]], target)
    assert(Sys.chmod(target, sprintf("%04o", as.integer(row$mode))), "Could not set member permissions.")
    checked_file(target, row)
    assert(as.integer(file.info(target)$mode) %% 512L == row$mode, "Prepared mode differs.")
    expected <- c(expected, row$path)
  }
  directories <- character()
  for (name in expected) {
    parent <- dirname(name)
    while (parent != ".") { directories <- c(directories, parent); parent <- dirname(parent) }
  }
  found <- list.files(output, recursive = TRUE, all.files = TRUE, no.. = TRUE, include.dirs = TRUE)
  assert(setequal(found, c(expected, unique(directories))), "Unexpected prepared directory entries.")
  for (name in found) no_links(file.path(output, name))
  no_links(output)
  invisible(output)
}
verify_sources <- function(root) {
  manifest <- read_json(file.path(root, "ci", "preserved-files.json"))
  rows <- c(manifest$files, manifest$approved_additions)
  for (row in rows) {
    path <- file.path(root, safe_member(row$path)); info <- regular(path)
    assert(row$mode %in% c("100644", "100755"), "Unsupported preserved mode.")
    assert(identical(sha256_file(path), row$sha256) &&
      identical(bitwAnd(as.integer(info$mode), 73L) != 0L, row$mode == "100755"),
      "Preserved source differs: ", row$path)
  }
  cat("Verified", length(rows), "preserved source files.\n")
}
reader_main <- function() {
  root <- reader_root(); args <- commandArgs(trailingOnly = TRUE)
  if (!length(args)) args <- "--verify"
  assert(args[[1L]] %in% c("--verify", "--list", "--extract", "--pars", "--pars-list"), "Use make verify, list, extract or saved-pars.")
  extract <- args[[1L]] %in% c("--extract", "--pars")
  assert(length(args) == if (extract) 2L else 1L, "Expected one new absolute output path for extraction.")
  if (extract) fresh_output(args[[2L]], root)
  if (args[[1L]] == "--verify") verify_sources(root)
  file <- if (args[[1L]] %in% c("--pars", "--pars-list")) "files.json" else "saved-results.json"
  manifest <- read_json(file.path(root, "reproduce", file))
  if (args[[1L]] %in% c("--pars", "--pars-list")) manifest <- list(schema_version = 1, archive = read_json(file.path(root, "reproduce", "package.json"))$archive, files = manifest$files)
  bundle <- checked_tar(root, manifest, keep = extract)
  if (args[[1L]] %in% c("--list", "--pars-list")) cat(paste(unique(sub("/.*$", "", vapply(bundle$rows, function(r) r$path, character(1L)))), collapse = "\n"), "\n", sep = "")
  else if (extract) { write_members(args[[2L]], bundle, root); cat("Extracted", length(bundle$rows), "original saved files to", args[[2L]], "\n") }
  else cat("Verified", length(bundle$rows), "original saved-result members; no model executed.\n")
}
if (sys.nframe() == 0L) reader_main()
