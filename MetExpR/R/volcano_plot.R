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
#'@param ngen s
#'@param fold_line s
#'@param title s
#'@param ylog s
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
#'@importFrom ggplot2 geom_vline
#'@importFrom grid unit
#'
#'@export

volcano_plot <- function(data, line=NA, names= NA,ylog=TRUE, log.fold,pval,id, ngen=NA, title=NA, fold_line=NA){
  
  if(ylog==TRUE){
    data$pval <- -log10(data$pval)
  }
    plot <- ggplot(data, aes(log.fold, pval)) +
      geom_point() +
      theme_bw(base_size = 12)
  
  if(is.na(title)){
    plot <- plot + ggtitle(paste0("Volcano plot of ",deparse(substitute(data))))
  }else{
    plot <- plot + ggtitle(paste0("Volcano plot of ",title) )
  }
  if(!is.na(fold_line)){
    plot <- plot+ geom_vline(xintercept=c(-fold_line,fold_line), col="red")
  }
  if(!is.na(line)){
    if(ylog==TRUE){plot <- plot + geom_hline(yintercept = -log10(line), col="red")+ylab("-log10(pval)")
    }else{
    plot <- plot + geom_hline(yintercept = line, col="red")}}
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
  if(!is.na(ngen)){
    plot <- plot +  geom_text_repel(
      data = subset(data, id==ngen),
      aes(label = id),
      size = 3,
      col = "red",
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    )+
      geom_point(data = subset(data, id==ngen), aes(log.fold, pval), col="red")
  }
  
  return(plot)
}
