#'@title boxplot_gene_expr
#'
#'@description Function \code{boxplot_gene_expr} generate a boxplot of expression for choosen gene.
#'
#'@param data data frame containing genes expression.
#'@param gene name of gene which expression we want to visualise.
#'
#'@return boxplot of expression.
#'
#'@seealso genereg_vs_met
#'
#'
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_boxplot
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 theme
#'@importFrom ggplot2 ggtitle
#'@importFrom ggplot2 element_blank
#'@importFrom ggplot2 xlab
#'
#'@export

boxplot_gene_expr <- function(data,gene){
  x <- data[,gene]
  x<-as.data.frame(x)
  colnames(x)[1] <- "expression"
  x$gene <- paste0(gene)
  colnames(x)[2] <- paste0(gene)
  plot1 <- ggplot(x,aes(paste0(gene),expression))+
    geom_boxplot()+
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
    ggtitle(paste0("Expression of ",gene))+
    xlab(paste0(gene))+
    theme_bw()
  return(plot1)
}
