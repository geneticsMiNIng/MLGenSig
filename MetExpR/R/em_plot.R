#'@title Visualise the p-values of expression and methylation for genes.
#'
#'@description Function \code{p_values_plot} draws a plot with p-values from methylation and expression when we use the t-test. In this case we testing the hypothesis that the means in both groups of "cancer type" are equal.
#'
#'@param data1 data.frame
#'@param condition condition fo groups
#'@param names condition fo groups
#'
#'@return plot
#'
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 geom_abline
#'@importFrom ggrepel geom_text_repel
#'@importFrom grid unit
#'@seealso vp_values_plot
#'
#'@export

em_plot <- function(data1, condition, names=NA){
  dataA <- data1[which(condition == unique(condition)[1]), ]
  dataB <- data1[which(condition == unique(condition)[2]), ]
  meanA <- sapply(dataA, mean)
  meanB <- sapply(dataB, mean)
  data <- as.data.frame(meanA)
  data$id <- rownames(data)
  data$meanB <- meanB
  plot <- ggplot(data, aes(x=meanA, y=meanB)) +
    geom_point() +
    geom_abline(slope = 1)+
    theme_bw()
  if(!is.na(names)) plot <- plot + geom_text_repel(
                                      data= tail(data[order(data$meanB-data$meanA), ], names),
                                      aes(label = id),
                                      size = 3,
                                      box.padding = unit(0.35, "lines"),
                                      point.padding = unit(0.3, "lines")
                                    )

  return(plot)

}
