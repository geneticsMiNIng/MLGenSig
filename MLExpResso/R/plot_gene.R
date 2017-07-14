#' @title Visualisations for genes.
#'
#' @description Function \code{plot_gene} generates a dashboard with methylation_path for methylation and boxplots for groups.
#'
#' @param condition.e condition for  expression
#' @param condition.m condition for methylation
#' @param data.m data for methylation
#' @param data.e data for expression
#' @param gene gene name
#'
#' @return A plot of class ggplot.
#'
#'@importFrom gridExtra tableGrob
#'@importFrom grid textGrob
#'@importFrom grid gpar
#'@importFrom gridExtra grid.arrange
#'@importFrom ggplot2 ggplot_gtable
#'@importFrom ggplot2 ggplot_build
#'
#' @export

plot_gene <- function(condition.e, condition.m, data.e, data.m, gene){
  title <- textGrob(gene, gp=gpar(fontsize = 25))
  g <- plot_methylation_path(data.m, condition.m, gene, observ = TRUE, show_gene = TRUE) +theme(legend.position = "none")
  b1 <- plot_diff_boxplot(data.e, gene, condition.e, sqrt.trans=TRUE, title=FALSE) +theme(legend.position = c(1,1), legend.justification=c(1,1))
  
  tmp <- ggplot_gtable(ggplot_build(b1))
  leg <- which(sapply(tmp$grobs, function(x) x$name) ==  "guide-box")
  legend <- tmp$grobs[[leg]]
  

  grid.arrange(title,legend,g,b1+ theme(legend.position = "none"),heights=unit(c(20,100),"mm"),
               layout_matrix =rbind(c(1 ,1 ,2 ,2),
                                    c(3 ,3 ,3 ,4)))

}

