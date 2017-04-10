#' @title genereg_vs_met2
#'
#' @description Function \code{genereg_vs_met2} ...
#'
#' @param data data frame containing values of methylation: columns coresponds to CpG islands, rows to samples.
#' @param gene vector of levels coresponding to order of samples in data.
#' @param condition vextor of subtype of cancer
#'
#' @return A plot.
#'
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_line
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 element_text
#'@importFrom ggplot2 geom_text
#'
#' @export

genereg_vs_met2 <-function(data,condition, gene){
  dataA <- data[which(condition == unique(condition)[1]), ]
  dataB <- data[which(condition == unique(condition)[2]), ]
  CpG_A <- CpG_mean(dataA, gene)
  CpG_A$condition <- unique(condition)[1]
  CpG_B <- CpG_mean(dataB, gene)
  CpG_B$condition <- unique(condition)[2]
  data2 <- rbind(CpG_A, CpG_B)

  n <- length(unique(data2$CPG_ISLAND_LOCATIONS))
  myPalette <- palette()[1:n]
  names(myPalette) <- unique(data2$CPG_ISLAND_LOCATIONS)

  plot1 <- ggplot(data2, aes(Name, mean, group=condition,colour=condition)) +
    geom_line()+
    geom_point(size=2)+
  theme_bw()+
    ggtitle(paste0("Methylation of gene ",gene))+
    xlab(paste0("Gene ",gene))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour=myPalette[as.character(data2$CPG_ISLAND_LOCATIONS)]),
          legend.position = "top")

  return(plot1)

}
