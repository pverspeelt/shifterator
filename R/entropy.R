#' Get Relative Frequency
#' 
#' Calculates the relative frequency (proportion) of each word in the data.frame
#'
#' @param data A data.frame containing words and their counts
#'
#' @return A data.frame containing words and their relative frequencies
#' 
#' @keywords internal

get_relative_frequency <- function(data) {
  # if length(class(data)) > 1 do as.data.frame
  # data <- as.data.frame(data)
  data$freq <- data[, 2] / sum(data[, 2])
  return(data[, c(1,3)])
}
