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
#' @export

visual <- function(condition.e, condition.m, data.m, data.e, gene, test.e, test.m){
  g <- genereg_vs_met(data.m, condition.m, gene, observ = TRUE, show_gen = TRUE)

  b1 <- boxplot_that(data.e,gene,condition.e)
  v.e <- volcano_plot(test.e, ngen = gene,ylog=TRUE, title="expression", fold_line = 2, line=0.05)


  data.m_gen <- map_to_gene(data.m)
  v.m <- volcano_plot(test.m,ngen = gene,ylog=TRUE,title="methylation",line=0.05)

  s <- tableGrob(t(sum_gen(data.e,condition.e , gene)))

  title <- textGrob("Summary", vjust = 5, gp=gpar(fontsize=20))

  grid.arrange(g,b1,v.e, v.m,
               layout_matrix =rbind(c(1 ,1 ,1 ,NA),
                                    c(1 ,1 ,1 ,2 ),
                                    c(1 ,1 ,1 ,2 ),
                                    c(3 ,3 ,4 ,4 ),
                                    c(3 ,3 ,4 ,4 ),
                                    c(3 ,3 ,4 ,4 )))

}

