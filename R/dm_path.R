
#' Path to a module
#' 
#' @param \dots Module name
#' 
#' @export
dm_path <- function(...) {
  dots <- list(...)
  base_path <- getOption("datamodules.artifacts")
  if (is.null(base_path)) {
    stop("Artifacts path not set, see `?setup_datamodules()`")
  }
  bits <- c(base_path, dots)
  do.call(file.path, bits)
}

#' Setup datamodules
#' 
#' Provides instructions for setting base path option
#' 
#' @param x 
#' 
#' @export
setup_datamodules <- function(x) {
  current_opt <- getOption("datamodules.artifacts")
  if (!is.null(current_opt)) {
    if (current_opt!=x) {
      warning(paste0(
        "found existing configuration, overwriting. To reset, run:\n",
        sprintf("  setup_datamodules(\"%s\")", current_opt)
      ))
    }
  }
  if (!dir.exists(x)) {
    stop(sprintf("directory '%s' does not exist, create it first", x))
  }
  cat("Setting base path for this R session\n")
  options(datamodules.artifacts = x)
  cat("To persist the path, add the following lines to your .Rprofile file:\n")
  cat("# datamodules base path for storing artifacts (data)\n")
  cat(sprintf("options(datamodules.artifacts = \"%s\")\n", x))
  if (requireNamespace("usethis", quietly = TRUE)) {
    cat("Opening .Rprofile for editing\n")
    usethis::edit_r_profile(scope = "user")
  }
  invisible(x)
}



