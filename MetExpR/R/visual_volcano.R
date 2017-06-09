#' @title Visulisations for methylation and expression.
#'
#' @description Function \code{visual_volcano} generate a dashboard with volcano plots for expression and methylation. Also it adds a tables with basic statistics.
#'
#' @param condition.e condition for  expression
#' @param condition.m condition for methylation
#' @param data.e data for expression
#' @param data.m data for methylation
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
#'@seealso volcano_plot, gene_stats
#'@export

visual_volcano <- function(condition.e, condition.m, data.e,data.m, gene, test.e=list(), test.m=list()){
  names.e <- names(test.e)
  names.m <- names(test.m)
  if(length(names.e)==0){
    names.e <- rep("",times=length(test.e))
  }
  if(length(names.m)==0){
    names.m <- rep("",times=length(test.m))

  }
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 1.8),
                bg_params = list(fill = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc"))),
    colhead = list(fg_params=list(cex = 1.8), bg_params=list(fill=c("white"))),
    rowhead = list(fg_params=list(cex = 1.8, fontface = "bold")))


  data.e.cpm <- as.data.frame(cpm(data.e))
  s.e <- tableGrob(t(gene_stats(data.e.cpm ,condition.e , gene)), theme = mytheme)
  data.m.map <- map_to_gene(data.m)
  s.m <- tableGrob(t(gene_stats(data.m.map,condition.m , gene)), theme = mytheme)

  title.e <- textGrob("Expression (cpm)", h = .9, gp=gpar(fontsize = 25))
  title.m <- textGrob("Methylation", h = .9, gp=gpar(fontsize = 25))

  title <- textGrob(gene, h = .9, gp=gpar(fontsize = 25), x = unit(1.1, "npc"))
  blank <- textGrob("", h = .9, gp=gpar(fontsize = 25))

  plist <- list(title, blank, title.m, title.e, s.m, s.e)
  if((length(test.e) + length(test.m)) > 0 ){
  l.e <- length(test.e)
  l.m <- length(test.m)

  for(i in 1:max(l.e, l.m)){
    if(i>l.e){
      plist[[length(plist)+1]] <- grid.rect(gp=gpar(col="white"))
    }else{
      plist[[length(plist)+1]] <- volcano_plot(test.e[[i]], ngen = gene,ylog=TRUE,title=names.e[i],fold_line = 2, line=0.05)
    }

    if(i>l.m){
      plist[[length(plist)+1]] <- grid.rect(gp=gpar(col="white"))
    }
    else{
      plist[[length(plist)+1]] <- volcano_plot(test.m[[i]], ngen = gene, title=names.m[i], ylog=TRUE, line=0.05)
    }

  }
  }
  heights.plots <- rep(130,max(length(test.e), length(test.m)))
  heights.g <- unit(c(20,10,30, heights.plots), "mm")
  grid.arrange(grobs = plist, ncol=2, heights=heights.g)

}
