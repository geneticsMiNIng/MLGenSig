#' @title genereg_vs_met
#'
#' @description Function \code{genereg_vs_met} ...
#'
#' @param data data frame containing values of methylation: columns coresponds to CpG islands, rows to samples.
#' @param gene vector of levels coresponding to order of samples in data.
#' @param condition vextor of subtype of cancer
#' @param show_gene s
#' @param observ s
#'
#' @return A plot of class ggplot.
#'
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_line
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 element_text
#'@importFrom ggplot2 geom_text
#'@importFrom ggplot2 scale_y_continuous
#'@importFrom ggplot2 geom_segment
#'@importFrom ggplot2 ylim
#'@importFrom reshape melt
#'@importFrom dplyr %>%
#' @export

genereg_vs_met <-function(data,condition, gene, show_gene=FALSE,observ=FALSE){
  dataA <- data[which(condition == unique(condition)[1]), ]
  dataB <- data[which(condition == unique(condition)[2]), ]
  data_gen <-map_to_gene(data)
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
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5))+
    scale_y_continuous(minor_breaks =c(0.00,1))+
    scale_color_manual(values=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628"))
  if(observ==TRUE){
    genom <- illumina_humanmethylation_27_data[,c(1,4,11)]
    MapInfo <- genom[,c(1:2)]
    genom_data <- genom[genom$Symbol==gene,]
    genom_data_2<- data[,which(colnames(data)%in% genom_data$Name)]
    genom_data_2 <- cbind(genom_data_2,condition)
    melted_val <- genom_data_2 %>% melt(id.vars="condition")
    colnames(melted_val) <- c("condition","Name","value")
    melted_val <- merge(melted_val,MapInfo, by="Name")
    plot1<- plot1+geom_point(data=melted_val, aes(MapInfo,value),size=0.7)
  }
  if(show_gen==TRUE){
    plot1 <- plot1 + geom_segment(aes(x=gene_loc[1], xend=gene_loc[2], y=0, yend=0), colour="blue", size=2)
  }
  return(plot1)

}
