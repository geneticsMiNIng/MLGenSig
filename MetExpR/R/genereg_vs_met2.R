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
  plot1 <- ggplot(CpG_A, aes(Name, mean, group=1,col=CPG_ISLAND_LOCATIONS)) +
    geom_point(size=3)+
    geom_line(col="black")+
    geom_point(data=CpG_B,aes(Name, mean, group=1,fill=CPG_ISLAND_LOCATIONS))+
    geom_line(data=CpG_B,aes(Name, mean, group=1))+
    geom_text(data= CpG_A,aes(label = condition))+
    geom_text(data=CpG_B, aes(label = condition))+
  theme_bw()+
    ggtitle(paste0("Methylation of gene ",gene))+
    xlab(paste0("Gene ",gene))+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")

  
  return(plot1)
  
}
