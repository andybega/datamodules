

.onAttach <- function(libname, pkgname) {
  base_path <- getOption("datamodules.artifacts")
  if (is.null(base_path)) {
    packageStartupMessage("Artifacts path not set, see `?setup_datamodules()`")
  } else {
    packageStartupMessage("Storing artifacts at '", base_path, "'")
  }
}