#' @title genereg_vs_met (tylko w układzie współrzędnych HG18 a nie HG19)
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
#'@importFrom grid arrow
#'@importFrom reshape2 melt
#'@importFrom dplyr %>%
#'@importFrom scales percent
#' @export


genereg_vs_met <-function(data,condition, gene, show_gene=FALSE,observ=FALSE, islands = TRUE){

  dataA <- data[which(condition == unique(condition)[1]), ]
  dataB <- data[which(condition == unique(condition)[2]), ]
  data_gen <-map_to_gene(data)
  CpG_A <- CpG_mean(dataA, gene)
  CpG_A$condition <- unique(condition)[1]
  CpG_B <- CpG_mean(dataB, gene)
  CpG_B$condition <- unique(condition)[2]
  data2 <- rbind(CpG_A, CpG_B)
  data2$Name_loc <- paste(data2$MapInfo, "\n",data2$Name)

  plot1 <- ggplot(data2, aes(MapInfo, mean, group=condition, colour=condition))+
#    geom_line()+
    theme_bw()+
#    ggtitle(paste0("Methylation of gene ",gene))+
    xlab(paste0("Gene ",gene))+
    theme(legend.position = "top",
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.border     = element_blank())+
    scale_y_continuous(limits =c(-0.05,1), expand = c(0.05, -0.05), labels=percent)+
    scale_color_manual(values=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628"))+
    ylab("")
  if(observ==TRUE){
    genom <- illumina_humanmethylation_27_data[,c(1,4,11)]
    MapInfo <- genom[,c(1:2)]
    genom_data <- genom[genom$Symbol==gene,]
    genom_data_2<- data[,which(colnames(data)%in% genom_data$Name)]
    genom_data_2 <- cbind(genom_data_2,condition)
    melted_val <- genom_data_2 %>% melt(id.vars="condition")
    colnames(melted_val) <- c("condition","Name","value")
    melted_val <- merge(melted_val,MapInfo, by="Name")
    plot1<- plot1+geom_point(data=melted_val, aes(MapInfo,value),size=0.5, alpha=0.35)
  }

  #Means over observations
  plot1 <- plot1 + geom_point(size=2.5)

  gene_loc <- gene_loc(gene)

  if(show_gene==TRUE){
    if(gene_loc[1] < min(data2$MapInfo)){
      plot1 <- plot1 + geom_segment(aes(x=max(gene_loc[1],min(data2$MapInfo))-1000, xend=min(gene_loc[2],max(data2$MapInfo)), y=-0.025, yend=-0.025), colour="blue", size=1, arrow=arrow(length = unit(0.3,"cm"),ends="first"))
    }
    if(gene_loc[2]> max(data2$MapInfo)){
      plot1 <- plot1 + geom_segment(aes(x=max(gene_loc[1],min(data2$MapInfo)), xend=min(gene_loc[2],max(data2$MapInfo))+1000, y=-0.025, yend=-0.025), colour="blue", size=1, arrow=arrow(length = unit(0.3,"cm"),ends="last"))
    }
    if((gene_loc[1] > min(data2$MapInfo))&&(gene_loc[2]> max(data2$MapInfo))){
      plot1 <- plot1 + geom_segment(aes(x=max(gene_loc[1],min(data2$MapInfo)), xend=min(gene_loc[2],max(data2$MapInfo)), y=-0.025, yend=-0.025), colour="blue", size=1)
    }
  }

  if(islands==TRUE){
    CpG_A_isl <- aggregate(CpG_A[,6], list(CpG_A$CPG_ISLAND_LOCATIONS), mean)
    colnames(CpG_A_isl) <- c("CPG_ISLAND_LOCATIONS","mean")
    CpG_A_isl$START <- sub(".*:", "", CpG_A_isl$CPG_ISLAND_LOCATIONS)
    CpG_A_isl$START <- sub("-.*", "", CpG_A_isl$START)
    CpG_A_isl$END <- sub(".*-", "", CpG_A_isl$CPG_ISLAND_LOCATIONS)
    CpG_A_isl_melt <- melt(CpG_A_isl, id.vars=c("CPG_ISLAND_LOCATIONS", "mean"))
    CpG_A_isl_melt$value <- as.numeric(as.character(CpG_A_isl_melt$value))
    CpG_A_isl_melt$condition <- unique(condition)[1]
    CpG_B_isl <- aggregate(CpG_B[,6], list(CpG_B$CPG_ISLAND_LOCATIONS), mean)
    colnames(CpG_B_isl) <- c("CPG_ISLAND_LOCATIONS","mean")
    CpG_B_isl$START <- sub(".*:", "", CpG_B_isl$CPG_ISLAND_LOCATIONS)
    CpG_B_isl$START <- sub("-.*", "", CpG_B_isl$START)
    CpG_B_isl$END <- sub(".*-", "", CpG_B_isl$CPG_ISLAND_LOCATIONS)
    CpG_B_isl_melt <- melt(CpG_B_isl, id.vars=c("CPG_ISLAND_LOCATIONS", "mean"))
    CpG_B_isl_melt$value <- as.numeric(as.character(CpG_B_isl_melt$value))
    CpG_B_isl_melt$condition <- unique(condition)[2]
    data3 <- rbind(CpG_A_isl_melt, CpG_B_isl_melt)
    data3$island_cond <- paste(data3$CPG_ISLAND_LOCATIONS, "\n",data3$condition)

    plot1 <- plot1 + geom_line(data=data3, aes(x=value, y=mean, group=island_cond, colour=condition))
  }
  return(plot1)

}
