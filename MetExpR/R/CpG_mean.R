#' @title CpG_mean
#'
#' @description Function \code{CpG_mean} computes methylation means of CpG islands for choosen gene. In this case: “BRCA1”
#'
#' @param data data frame containing values of methylation: columns coresponds to CpG islands, rows to samples.
#' @param gene name of choosen gene.
#'
#' @return Data frame containing
#'
#'@export

CpG_mean <-function(data, gene){
  CpG <- illumina_humanmethylation_27_data[which(illumina_humanmethylation_27_data$Symbol==gene), c(1,4,11,18,19)]
  methy <- data[ ,which(colnames(data) %in% CpG$Name)]
  methy.mean <- sapply(methy, mean, na.rm=TRUE)
  names(methy.mean) <- colnames(methy)
  CpG$mean <- methy.mean[as.character(CpG$Name)]
  CpG <- CpG[which(!is.na(CpG$mean)), ]
  CpG$Name <- factor(CpG$Name, levels = CpG$Name)

  return(CpG)
}

