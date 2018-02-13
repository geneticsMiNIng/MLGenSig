#' @title CpG_mean
#'
#' @description Function \code{calculate_CpG_mean} computes methylation means of CpG probes for chosen gene.
#'
#' @param data data frame containing values of methylation: columns coresponds to CpG probes, rows to samples.
#' @param gene name of chosen gene.
#' @param genom.data #' @param genom.data Data frame which contains information about CpG probes and corresponding genes, by default in our package we use \code{\link{illumina_humanmethylation_27_data}}. New dataset should contain 5 columns with:
#' 1) CpG probe names, 2) CpG probe locations, 3) gene names, 4) logical value if there is a CpG island, 5) location of island.
#'
#' @return Data frame containing following columns:
#'  \item{Name}{The name of CpG probe.}
#'  \item{HG18_coord}{Probes coordinates from human build GRC36(hg18).}
#'  \item{Symbols}{Name of chosen gene.}
#'  \item{CPG_ISLANDS}{Logical value if probe belongs to some CpG Island.}
#'  \item{CPG_ISLANDS_LOCATIONS}{Location of CpG island to which probe belongs .}
#'  \item{mean}{Mean methylation level compute for all samples of data.}
#'
#' @keywords internal

calculate_CpG_mean <- function(data, gene, genom.data=NULL) {
  CpG <- genom.data[which(genom.data$Symbol == gene), ]
  methy <- data[, which(colnames(data) %in% CpG$Name)]
  if (class(methy) != "data.frame") {
    methy <- as.data.frame(methy)
    names <- CpG$Name
    logical <- (CpG$Name %in% colnames(data))
    x <- names[logical]
    x <- as.data.frame(x)
    colnames(methy) <- x[1, 1]
  }
  methy.mean <- sapply(methy, mean, na.rm = TRUE)
  names(methy.mean) <- colnames(methy)
  CpG$mean <- methy.mean[as.character(CpG$Name)]
  CpG <- CpG[which(!is.na(CpG$mean)), ]
  CpG$Name <- factor(CpG$Name, levels = CpG$Name)

  return(CpG)
}
