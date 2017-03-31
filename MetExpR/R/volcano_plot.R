#'Visualise the p-values of expression and methylation for genes.
#'
#'Function \code{volcano_plot} draws a plot with p-values and fold logarithm from methylation or expression when we use the t-test.
#'
#'
#'@param dt data frame with p-values from testing difference in cancer groups.
#'@param type we can choose a plot for methylation of expression. 
#'@param exp.log.fold logarithm of expression fold.
#'@param met.log.fold logarithm of methylation fold.
#'@param exp.pval p-value for expression.
#'@param met.pval p-value for methylation.
#'@param id vector of genes symbols.
#'
#'@return plot
#'
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 scale_color_manual
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 geom_hline
#'@importFrom ggrepel geom_text_repel
#'@importFrom grid unit
#'
#'@export

volcano_plot <- function(dt,type,exp.log.fold,exp.pval,met.log.fold,met.pval,id){
  if(type=="expression"){
  plot1 <- ggplot(dt, aes(exp.log.fold, -log10(exp.pval))) + 
    geom_point() +
    scale_color_manual(values = c("red", "grey")) +
    theme_bw(base_size = 12) +
    geom_text_repel(
      data = subset(dt, exp.pval < 0.05 & met.pval < 0.05),
      aes(label = id),
      size = 3,
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    )+
    geom_hline(yintercept = -log10(0.05), col="red")+
    ggtitle("Volcano plot of expression")
  return(plot1)
  }
  if(type=="methylation"){
    plot1 <- ggplot(dt, aes(met.log.fold, -log10(met.pval))) + 
      geom_point() +
      scale_color_manual(values = c("red", "grey")) +
      theme_bw(base_size = 12) +
      geom_text_repel(
        data = subset(dt, exp.pval < 0.05 & met.pval < 0.05),
        aes(label = id),
        size = 3,
        box.padding = unit(0.35, "lines"),
        point.padding = unit(0.3, "lines")
      )+
      geom_hline(yintercept = -log10(0.05), col="red")+
      ggtitle("Volcano plot of methylation")
    return(plot1)
  }
  else{
    cat("Wrong type of plot.")
  }
  
  
}