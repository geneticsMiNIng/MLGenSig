#' @title genereg_vs_met2
#'
#' @description Function \code{genereg_vs_met2} ...
#'
#' @param data data frame containing values of methylation: columns coresponds to CpG islands, rows to samples.
#' @param gene vector of levels coresponding to order of samples in data.
#' @param condition vextor of subtype of cancer
#'
#' @return A plot of class ggplot.
#'
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_line
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 element_text
#'@importFrom ggplot2 geom_text
#'@importFrom ggplot2 scale_x_continuous
#'@importFrom ggplot2 geom_segment
#'@importFrom ggplot2 ylim
#' @export

genereg_vs_met2 <-function(data,condition, gene){
  dataA <- data[which(condition == unique(condition)[1]), ]
  dataB <- data[which(condition == unique(condition)[2]), ]
  CpG_A <- CpG_mean(dataA, gene)
  CpG_A$condition <- unique(condition)[1]
  CpG_B <- CpG_mean(dataB, gene)
  CpG_B$condition <- unique(condition)[2]
  data2 <- rbind(CpG_A, CpG_B)
  data2$Name_loc <- paste(data2$MapInfo, "\n",data2$Name)
  gene_loc <- gen_loc(gene)

  plot1 <- ggplot(data2, aes(MapInfo, mean, group=condition, colour=condition))+
    geom_line()+
    geom_point(size=2)+
  theme_bw()+
    ggtitle(paste0("Methylation of gene ",gene))+
    xlab(paste0("Gene ",gene))+
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 90, hjust = 0))+
    scale_x_continuous(breaks=data2$MapInfo, labels=data2$Name)+
    ylim(0,1)
  if((min(data2$MapInfo) < gene_loc[1]) && (max(data2$MapInfo) > gene_loc[2]) ){
    plot1 <- plot1 + geom_segment(aes(x=gene_loc[1], xend=gene_loc[2], y=0, yend=0), colour="blue", size=2)
  }
  return(plot1)

}
