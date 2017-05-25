#' @title visual
#'
#' @description Function ...
#'
#' @param condition.e condition for  expression
#' @param condition.m condition for methylation
#' @param data.m data for methylation
#' @param data.e data for expression
#' @param gene gene name
#' @param test.e test results for expression
#' @param test.m test results for methylation
#'
#' @return A plot of class ggplot.
#'
#'@importFrom gridExtra tableGrob
#'@importFrom grid textGrob
#'@importFrom grid gpar
#'@importFrom gridExtra grid.arrange
#'@importFrom edgeR cpm
#'
#' @export

visual_gene <- function(condition.e, condition.m, data.m, data.e, gene, test.e, test.m){
  title <- textGrob(gene, gp=gpar(fontsize = 25))
  g <- genereg_vs_met(data.m, condition.m, gene, observ = TRUE, show_gen = TRUE) +theme(legend.position = "none")
  data.e.cpm <- cpm(data.e)
  b1 <- boxplot_that(data.e.cpm, gene, condition.e) +theme(legend.position = c(1,1), legend.justification=c(1,1))

  grid.arrange(title,g,b1,heights=unit(c(20,100),"mm"),
               layout_matrix =rbind(c(1 ,1 ,1 ,1),
                                    c(2 ,2 ,2 ,3)))

}

