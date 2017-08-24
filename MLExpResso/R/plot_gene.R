#' @title Visualisations for genes.
#'
#' @description Function \code{plot_gene} generates a dashboard with methylation_path for methylation and boxplots for groups.
#'
#' @param data.m data frame containing information from methylation.
#' @param data.e data frame containing information from expression.
#' @param condition.m condition for methylation data (data.m).
#' @param condition.e condition for expression data (data.e).
#' @param gene gene name.
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
#'library(MLExpRessoData)
#'condition_exp <- ifelse(BRCA_exp$SUBTYPE == "LumA","LumA","other")
#'condition_met <- ifelse(BRCA_met$SUBTYPE == "LumA","LumA","other")
#'plot_gene(BRCA_met,BRCA_exp,condition_met, condition_exp, "ICAM2")
#'}
#'
#'@seealso \code{\link{plot_methylation_path}}, \code{\link{plot_diff_boxplot}}
#' @export

plot_gene <- function(data.m, data.e, condition.m, condition.e, gene){
  title <- textGrob("", gp=gpar(fontsize = 25))
  g <- plot_methylation_path(data.m, condition.m, gene, observ = TRUE, show_gene = TRUE, title=TRUE) +theme(legend.position = "none")
  b1 <- plot_diff_boxplot(data.e, condition.e, gene, sqrt.trans=TRUE, title=FALSE)
  #blank <- textGrob("", gp=gpar(fontsize = 25))


  grid.arrange(title,g,b1,heights=unit(c(20,100),"mm"),
               layout_matrix =rbind(c(1 ,1 ,1 ,1),
                                    c(3 ,3 ,3 ,4)))

}

