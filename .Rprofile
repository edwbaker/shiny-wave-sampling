source("renv/activate.R")

# Ensure cellar/ is not gitignored by renv
local({
  fix_cellar_gitignore <- function() {
    gi <- "renv/.gitignore"
    if (file.exists(gi)) {
      lines <- readLines(gi)
      if ("cellar/" %in% lines) {
        lines <- lines[lines != "cellar/"]
        writeLines(lines, gi)
      }
    }
  }
  # Fix on session start
  fix_cellar_gitignore()
  # Override renv::snapshot to fix after each call
  if (requireNamespace("renv", quietly = TRUE)) {
    original_snapshot <- renv::snapshot
    assignInNamespace("snapshot", function(...) {
      result <- original_snapshot(...)
      fix_cellar_gitignore()
      invisible(result)
    }, ns = "renv")
  }
})
