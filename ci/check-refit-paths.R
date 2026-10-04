# Test only the path guard; no package, model data or native execution.
expressions <- parse(file = "reproduce/refit.R")
helpers <- new.env(parent = baseenv())
for (name in c("fail", "absolute_clean")) {
  selected <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
                       identical(x[[2L]], as.name(name)), as.list(expressions))
  stopifnot(length(selected) == 1L)
  eval(selected[[1L]], envir = helpers)
}
valid <- file.path(normalizePath(tempdir(), mustWork = TRUE), "new-refit")
stopifnot(identical(helpers$absolute_clean(valid), valid))
invalid <- c("", "/", "relative", paste0(dirname(valid), "//new-refit"),
             paste0(dirname(valid), "/./new-refit"),
             paste0(dirname(valid), "/../new-refit"), paste0(valid, "\n"))
for (path in invalid) {
  result <- tryCatch(helpers$absolute_clean(path), error = identity)
  stopifnot(inherits(result, "error"))
}
target <- file.path(tempdir(), "original-input")
link <- file.path(tempdir(), "input-link")
stopifnot(dir.create(target), file.symlink(target, link))
result <- tryCatch(helpers$absolute_clean(file.path(link, "new-refit")), error = identity)
stopifnot(inherits(result, "error"), !file.exists(valid))
cat("Refit path guard passed; no model execution.\n")
