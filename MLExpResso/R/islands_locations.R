#' @title Locations of CpG islands
#'
#' @description ddd
#'
#' @param  data_A d
#' @param data_B d
#' @param condition vector with conditions
#'
#' @importFrom reshape2 melt
#' @importFrom stats aggregate
#' @return data frame
#'
#' @keywords internal

islands_locations <- function(data_A, data_B, condition) {
  CpG_A_isl <- aggregate(data_A[, 6], list(data_A$CPG_ISLAND_LOCATIONS), mean)
  colnames(CpG_A_isl) <- c("CPG_ISLAND_LOCATIONS", "mean")
  CpG_A_isl$START <- sub(".*:", "", CpG_A_isl$CPG_ISLAND_LOCATIONS)
  CpG_A_isl$START <- sub("-.*", "", CpG_A_isl$START)
  CpG_A_isl$END <- sub(".*-", "", CpG_A_isl$CPG_ISLAND_LOCATIONS)
  CpG_A_isl_melt <- melt(CpG_A_isl, id.vars = c("CPG_ISLAND_LOCATIONS", "mean"))
  CpG_A_isl_melt$value <- as.numeric(as.character(CpG_A_isl_melt$value))
  CpG_A_isl_melt$condition <- unique(condition)[1]
  CpG_B_isl <- aggregate(data_B[, 6], list(data_B$CPG_ISLAND_LOCATIONS), mean)
  colnames(CpG_B_isl) <- c("CPG_ISLAND_LOCATIONS", "mean")
  CpG_B_isl$START <- sub(".*:", "", CpG_B_isl$CPG_ISLAND_LOCATIONS)
  CpG_B_isl$START <- sub("-.*", "", CpG_B_isl$START)
  CpG_B_isl$END <- sub(".*-", "", CpG_B_isl$CPG_ISLAND_LOCATIONS)
  CpG_B_isl_melt <- melt(CpG_B_isl, id.vars = c("CPG_ISLAND_LOCATIONS", "mean"))
  CpG_B_isl_melt$value <- as.numeric(as.character(CpG_B_isl_melt$value))
  CpG_B_isl_melt$condition <- unique(condition)[2]
  data3 <- rbind(CpG_A_isl_melt, CpG_B_isl_melt)
  data3$island_cond <- paste(data3$CPG_ISLAND_LOCATIONS, "\n", data3$condition)
  return(data3)
}
