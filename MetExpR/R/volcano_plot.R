#'@title Visualise the p-values of expression and methylation for genes.
#'
#'@description Function \code{volcano_plot} draws a plot with p-values and fold logarithm from methylation or expression when we use the t-test.
#'
#'
#'@param data data.frame, a result of `expr_nbinom` function.
#'@param line p-value on which we draw a line.
#'@param names p-value below which...
#'@param log.fold logarithm of fold.
#'@param pval p-value.
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

volcano_plot <- function(data, line=NA, names= NA, log.fold,pval,id){
  plot <- ggplot(data, aes(log.fold, -log10(pval))) +
    geom_point() +
    scale_color_manual(values = c("red", "grey")) +
    theme_bw(base_size = 12) +
    ggtitle(paste0("Volcano plot of ",deparse(substitute(data))))

  if(!is.na(line)) plot <- plot + geom_hline(yintercept = -log10(line), col="red")
  if(!is.na(names) & names < 1) plot <- plot +     geom_text_repel(
                                          data = subset(data, pval < names),
                                          aes(label = id),
                                          size = 3,
                                          box.padding = unit(0.35, "lines"),
                                          point.padding = unit(0.3, "lines")
                                        )
  if(!is.na(names) & names >= 1) plot <- plot + geom_text_repel(
    data = head(data[order(data$pval), ], names),
    aes(label = id),
    size = 3,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )
  return(plot)
}
