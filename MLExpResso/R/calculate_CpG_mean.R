#'@title CpG_mean
#'
#'@description Function \code{calculate_CpG_mean} computes methylation means of CpG probes for chosen gene.
#'
#'@param data data frame containing values of methylation: columns coresponds to CpG probes, rows to samples.
#'@param gene name of chosen gene.
#'
#'@return Data frame containing following columns:
#'  \item{Name}{The name of CpG probe.}
#'  \item{HG18_coord}{Probes coordinates from human build GRC36(hg18).}
#'  \item{Symbols}{Name of chosen gene.}
#'  \item{CPG_ISLANDS}{Logical value if probe belongs to some CpG Island.}
#'  \item{CPG_ISLANDS_LOCATIONS}{Location of CpG island to which probe belongs .}
#'  \item{mean}{Mean methylation level compute for all samples of data.}
#' 

calculate_CpG_mean <-function(data, gene){
  illumina_humanmethylation_27_data <- MLExpResso::illumina_humanmethylation_27_data
  CpG <- illumina_humanmethylation_27_data[which(illumina_humanmethylation_27_data$Symbol==gene), c(1,4,11,18,19)]
  methy <- data[ ,which(colnames(data) %in% CpG$Name)]
  if(class(methy)!="data.frame"){
      methy <- as.data.frame(methy)
      names <- CpG$Name
      logical <- (CpG$Name %in% colnames(data))
      x <- names[logical]
      x <- as.data.frame(x)
      colnames(methy) <- x[1,1]
    }
  methy.mean <- sapply(methy, mean, na.rm=TRUE)
  names(methy.mean) <- colnames(methy)
  CpG$mean <- methy.mean[as.character(CpG$Name)]
  CpG <- CpG[which(!is.na(CpG$mean)), ]
  CpG$Name <- factor(CpG$Name, levels = CpG$Name)

  return(CpG)
}

