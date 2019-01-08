source("R/utils_packages_loader.R")


#' Fit a 1-dimensional Ornstein-Uhlenbeck model to the data.
#' @param times 1D vector of timestamps.
#' @param data 1D vector of values to fit.
#' @param threshold Value to separate continuous and
#'     jump parts.
#' @return 1D OU parameter value. It should be positive.
#' @examples
#' a <- 2
#' sigma <- 0.1
#' times <- 1:10000
#' data <- rep(0, 10000)
#' for(i in 2:10000){
#'     data[i] <- data[i-1] - a * data[i-1] +
#'                rnorm(n=1, sd=sigma)
#' }
#'
#' NOUfit1D(times = times, data = data, threshold = 1)
NOUfit1D <- function(times, data, threshold){
  N <- length(data)
  if(threshold <= 0){
    stop('threshold should be positive.')
  }

  diff_filtered <- DataFiltering(data,
                                 thresholds = threshold,
                                 diff_values=T)
  diff_times <- TimeMatrix(times = times, ncol=1)

  mle_estimate_up <- data[-N] * diff_filtered
  mle_estimate_down <- data[-N]^2 * diff_times
  return(-sum(mle_estimate_up) / sum(mle_estimate_down))
}
