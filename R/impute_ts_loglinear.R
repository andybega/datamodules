#' Log-linear imputer
#' 
#' @param x a time series
#' 
#' @examples 
#' # Infant mortality in PRK
#' x <- structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
#'   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 27.7, 27.4, 27.8, 
#'   28.9, 30.7, 33.4, 36.9, 41.1, 45.5, 49.8, 53.2, 55.1, 55, 53.1, 
#'   49.4, 44.5, 39.1, 34.2, 30.4, 27.8, 26.4, 26, 25.7, 25.2, 24.4, 
#'   23.1, 21.5, 19.8, 18.3, 17, 16, 15.1, 14.4), 
#'   label = "Mortality rate, infant (per 1,000 live births)")
#' plot(x, type = "l", ylim = c(0, 70))
#' lines(impute_ts_loglinear(x), col = "red", lty = 3)
#' 
#' @export
impute_ts_loglinear <- function(x) {
  if (all(is.na(x))) {
    return(x)
  }
  xx <- seq_along(x)
  mdl <- try(glm(x ~ xx, data = NULL, family = gaussian(link = "log")), silent = TRUE)
  if (inherits(mdl, "try-error")) {
    return(x)
  }
  xhat <- predict(mdl, newdata = list(xx = xx), type = "response")
  x[is.na(x)] <- xhat[is.na(x)]
  x
}