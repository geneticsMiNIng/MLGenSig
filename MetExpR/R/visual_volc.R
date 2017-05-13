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

visual_volc <- function(condition.e, condition.m, data.m, data.e, gene, test.e, test.m){

  v.e <- volcano_plot(test.e, ngen = gene,ylog=TRUE, title="expression", fold_line = 2, line=0.05)
  v.m <- volcano_plot(test.m,ngen = gene,ylog=TRUE,title="methylation",line=0.05)

  s <- tableGrob(t(sum_gen(data.e,condition.e , gene)))

  title <- textGrob("Summary", vjust = 5, gp=gpar(fontsize=20))

  grid.arrange(v.m, v.e,
               layout_matrix =rbind(c(1 ,1 ,2,2)))

}

