#'@title Visualise the p-values of expression and methylation for genes.
#'
#'@description Function \code{p_values_plot} draws a plot with p-values from methylation and expression when we use the t-test. In this case we testing the hypothesis that the means in both groups of "cancer type" are equal.
#'
#'@param dt_expr data.frame, a result of `expr_nbinom` function.
#'@param dt_met data.frame, a result of `mety_ttest` function.
#'@param names p-value below which...
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
#'@seealso volcano_plot
#'
#'@export

p_values_plot <- function(dt_expr,dt_met, names=NA, exp.pval,met.pval,id){
  dt <- full_data(dt_expr,dt_met)
  plot <- ggplot(dt, aes(x=-log(exp.pval), y=-log(met.pval))) +
    geom_point()+
    theme_bw()+
    ggtitle("P-values comparison")

  if(!is.na(names) & names<1) plot <- plot + geom_text_repel(
                                              data=subset(dt, met.pval<names | exp.pval<names),
                                              aes(label = id),
                                              size = 3,
                                              box.padding = unit(0.35, "lines"),
                                              point.padding = unit(0.3, "lines")
                                            )
  if(!is.na(names) & names>=1) plot <- plot + geom_text_repel(
                                                data= head(dt[order(dt$met.pval+dt$exp.pval), ], names),
                                                aes(label = id),
                                                size = 3,
                                                box.padding = unit(0.35, "lines"),
                                                point.padding = unit(0.3, "lines")
                                              )

  return(plot)

}
