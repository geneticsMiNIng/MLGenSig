#' @title visual
#'
#' @description Function ...
#'
#' @param condition.e condition for  expression
#' @param condition.m condition for methylation
#' @param data.m data for methylation
#' @param data.e data for expression
#' @param gene gene name
#' @param list.test.e list of tests results for expression
#' @param list.test.m list of tests results for methylation
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

  data.e.cpm <- as.data.frame(cpm(data.e))
  s.e <- tableGrob(t(sum_gen(data.e.cpm ,condition.e , gene)))
  data.m.map <- map_to_gene(data.m)
  s.m <- tableGrob(t(sum_gen(data.m.map,condition.m , gene)))

  title.e <- textGrob("Expression (cpm)", h = .9, gp=gpar(fontsize=10))
  title.m <- textGrob("Methylation", h = .9, gp=gpar(fontsize=10))

  plist <- list(title.e, title.m, s.e, s.m)

  #volcanos expression
  for(i in 1:length(test.e)){
    plist[[length(plist)+1]] <- volcano_plot(test.e[[i]], ngen = gene,ylog=TRUE, title="expression", fold_line = 2, line=0.05)
  }
  #volcanos methylation
  for(i in 1:length(test.m)){
    plist[[length(plist)+1]] <- volcano_plot(test.m[[i]], ngen = gene,ylog=TRUE, title="expression", line=0.05)
  }

  heights.plots <- rep(100,max(length(test.e), length(test.m)))
  heights.g <- unit(c(10,100, heights.plots), "mm")
  grid.arrange(grobs = plist, ncol=2, heights=heights.g)

}

