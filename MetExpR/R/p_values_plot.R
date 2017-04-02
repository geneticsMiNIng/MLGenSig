#'Visualise the p-values of expression and methylation for genes.
#'
#'Function \code{p_values_plot} draws a plot with p-values from methylation and expression when we use the t-test. In this case we testing the hypothesis that the means in both groups of "cancer type" are equal.
#'
#'@param dt data frame with p-values from testing difference in cancer groups.
#'@param exp.pval p-value for expression.
#'@param met.pval p-value for methylation.
#'@param id vector of genes symbols.
#'
#'@return plot
#'
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 ggtitle
#'@importFrom ggrepel geom_text_repel
#'@importFrom grid unit
#'
#'
#'@export

p_values_plot <- function(dt,exp.pval,met.pval,id){
  plot1 <- ggplot(dt, aes(x=-log(exp.pval), y=-log(met.pval))) + 
    geom_point()+
    theme_bw()+
    geom_text_repel(
      data=subset(dt, -log(met.pval)>20 | -log(exp.pval)>20),
      aes(label = id),
      size = 3,
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    )+
    ggtitle("P-values comparison")
  return(plot1)
  
}
