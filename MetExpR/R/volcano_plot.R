#'@title Visualise the p-values of expression and methylation for genes.
#'
#'@description Function \code{volcano_plot} draws a plot with p-values and fold logarithm from methylation or expression when we use the t-test.
#'
#'
#'@param data data.frame, a result of `expr_nbinom` function.
#'@param line p-value on which we draw a line.
#'@param names p-value below which...
#'@param log2.fold logarithm of fold.
#'@param pval p-value.
#'@param id vector of genes symbols.
#'@param ngen symol or vector of gene names
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
#'@importFrom ggplot2 scale_x_continuous
#'@importFrom ggplot2 scale_y_continuous
#'@importFrom grid unit
#'@importFrom scales trans_breaks
#'@importFrom scales trans_format
#'@importFrom ggthemes extended_range_breaks
#'@importFrom scales math_format
#'@importFrom scales trans_new
#'@export

volcano_plot <- function(data, line=NA, names= NA,ylog=TRUE, log2.fold,pval,id, ngen=NA, title=NA, fold_line=NA){

  if(ylog==TRUE){
    #data$pval <- log10(data$pval)
    data$pval <- data$pval
  }
    plot <- ggplot(data, aes(log2.fold, pval)) +
      geom_point(size = 0.5) +
      theme_bw(base_size = 12)+
      theme(panel.border = element_blank(),
            axis.text.x = element_text(size=15),
            axis.text.y = element_text(size=15))+
      scale_y_continuous(trans= reverselog_trans(10),
                         breaks = trans_breaks("log10", function(x) 10^x),
                         labels = trans_format("identity", math_format(10^.x)))+
      scale_x_continuous(breaks = extended_range_breaks()(data$log2.fold),
                         labels = function(x) sprintf("%.1f", x))

  if(is.na(title)){
    plot <- plot + ggtitle("")
  }else{
    plot <- plot + ggtitle(paste0(title) )
  }
  if(!is.na(fold_line)){
    plot <- plot+ geom_vline(xintercept=c(-fold_line,fold_line), col="red")
  }
  if(!is.na(line)){
    if(ylog==TRUE){plot <- plot + geom_hline(yintercept = line, col="red")+ylab("-log10(pval)")
    }else{
    plot <- plot + geom_hline(yintercept = 10^(line), col="red")}}
  if(!is.na(names) & names < 1) plot <- plot +     geom_text_repel(
                                          data = subset(data, pval < names),
                                          aes(label = id),
                                          size = 3,
                                          col = "grey",
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
    data2 <- data[which(data$id %in% ngen),]
    plot <- plot +
      geom_point(data=data2, aes(log2.fold, pval), col="red", size=2)
    if(length(ngen)>1){
    plot <- plot +
      geom_text_repel(
        data = data[which(data$id %in% ngen),],
        aes(label = id),
        size = 3,
        col="grey",
        box.padding = unit(0.35, "lines"),
        point.padding = unit(0.3, "lines")
      )
    }
  }

  return(plot)
}
