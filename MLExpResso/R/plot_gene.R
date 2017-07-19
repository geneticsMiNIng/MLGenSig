#' @title Visualisations for genes.
#'
#' @description Function \code{plot_gene} generates a dashboard with methylation_path for methylation and boxplots for groups.
#'
#' @param data.m data for methylation
#' @param data.e data for expression
#' @param condition.m condition for methylation
#' @param condition.e condition for  expression
#' @param gene gene name
#'
#' @return Object of class ggplot containing visualisation of methylation on gene and boxplot for values from choosen gene broken down to groups.
#'
#'@importFrom gridExtra tableGrob
#'@importFrom grid textGrob
#'@importFrom grid gpar
#'@importFrom gridExtra grid.arrange
#'@importFrom ggplot2 ggplot_gtable
#'@importFrom ggplot2 ggplot_build
#'
#'@seealso \code{\link{plot_methylation_path}}, \code{\link{plot_diff_boxplot}}
#'
#'@examples
#'\dontrun{
#'condition_exp <- ifelse(BRCA_mRNAseq_chr17$SUBTYPE=="LumA","LumA","other")
#'condition_met <- ifelse(BRCA_methylation_chr17$SUBTYPE=="LumA","LumA","other")
#'plot_gene(BRCA_methylation_chr17,BRCA_mRNAseq_chr17,condition_met, condition_exp, "ICAM2")
#'}
#'
#'@seealso \code{\link{plot_methylation_path}}, \code{\link{plot_diff_boxplot}}
#' @export

plot_gene <- function(data.m, data.e, condition.m, condition.e, gene){
  title <- textGrob(gene, gp=gpar(fontsize = 25), just="left")
  g <- plot_methylation_path(data.m, condition.m, gene, observ = TRUE, show_gene = TRUE) +theme(legend.position = "none")
  b1 <- plot_diff_boxplot(data.e, gene, condition.e, sqrt.trans=TRUE, title=FALSE) 



  grid.arrange(title,g,b1,heights=unit(c(20,100),"mm"),
               layout_matrix =rbind(c(1 ,1 ,1 ,1),
                                    c(3 ,3 ,3 ,4)))

}

