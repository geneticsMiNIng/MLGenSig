#' @title Locations of CpG islands
#'
#' @description ddd
#'
#' @param  data2 d
#'
#' @importFrom stats aggregate
#' @return data frame
#'
#' @keywords internal

islands_locations <- function(data2) {

  res <- data2
  res$START <- sub(".*:", "", res$CPG_ISLAND_LOCATIONS)
  res$START <- sub("-.*", "", res$START)
  res$END <- sub(".*-", "", res$CPG_ISLAND_LOCATIONS)
  res$mean_condition <- paste0(res$condition,res$START)
  aggregated_res <- aggregate(res[,6], list(condition=res$condition,
                                            START=res$START,
                                            END=res$END), mean)
  colnames(aggregated_res)[4] <- "mean"

  return(aggregated_res)
}


