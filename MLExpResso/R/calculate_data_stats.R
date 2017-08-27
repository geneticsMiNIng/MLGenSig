#' @title Statistics for data
#'
#' @description bla
#'
#' @param data dataset containing interesting values
#' @param condition vector of conditions
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' library(MLExpRessoData)
#'
#' condition_m <- ifelse(BRCA_methylation_all == "LumA", "LumA", "other")
#'
#' stats <- calculate_stats(BRCA_methylation_all, condition_m)
#' }
#'
#' @keywords internal

calculate_data_stats <- function(data, condition) {
  nvar <- as.data.frame(ncol(data))
  colnames(nvar) <- "Variables"

  nsample <- as.data.frame(nrow(data))
  colnames(nsample) <- "Samples"

  condition_freq <- t(as.data.frame(table(condition)))
  colnames(condition_freq) <- condition_freq[1, ]
  condition_freq <- t(as.data.frame(condition_freq[2, ]))

  result <- cbind(nvar, nsample, condition_freq)
  rownames(result) <- NULL

  return(result)
}
