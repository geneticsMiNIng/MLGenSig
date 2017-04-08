#' @title genereg_vs_met
#'
#' @description Function \code{genereg_vs_met} ...
#'
#' @param data data frame containing values of methylation: columns coresponds to CpG islands, rows to samples.
#' @param gene vector of levels coresponding to order of samples in data.
#' @param gr group of subtype of cancer
#'
#' @return A plot.
#'
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_line
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 element_text
#'
#' @export

genereg_vs_met <-function(data, gene, gr=NULL){
  CpG <- CpG_mean(data, gene)
  plot <- ggplot(CpG, aes(Name, mean, group=1,col=CPG_ISLAND_LOCATIONS)) +
            geom_point(size=4)+
            geom_line(col="black")+
            theme_bw()+
          ggtitle(paste0("Methylation of gene ",gene))+
          xlab(paste0("Gene ",gene))+ 
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")
    
          
    
  return(plot)

}