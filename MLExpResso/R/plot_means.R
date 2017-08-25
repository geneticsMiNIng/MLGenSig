#' @title Visualise the p-values for genes.
#'
#' @description Function \code{plot_means} draws a plot with p-values from methylation when we use the t-test. We testing the equality of means in given two groups.
#'
#' @param data1 data frame consisting the data from methylation or expresion
#' @param condition condition for groups
#' @param names number of values we want to mark on plot; in this case we decide to mark values with the smallest differences between means in groups.
#'
#' @return plot of p-values
#'
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_abline
#' @importFrom ggrepel geom_text_repel
#' @importFrom grid unit
#' @importFrom utils head
#' @importFrom utils tail
#'
#' @seealso p_values_plot
#'


plot_means <- function(data1, condition, names=NA) {
  id <- NULL
  dataA <- data1[which(condition == unique(condition)[1]), ]
  dataB <- data1[which(condition == unique(condition)[2]), ]
  meanA <- sapply(dataA, mean)
  meanB <- sapply(dataB, mean)
  data <- as.data.frame(meanA)
  data$id <- rownames(data)
  data$meanB <- meanB
  plot1 <- ggplot(data, aes(x = meanA, y = meanB)) +
    geom_point() +
    geom_abline(slope = 1) +
    theme_bw() +
    ggtitle(paste0("Means in groups from ", deparse(substitute(data1)))) +
    xlab(paste0("Group ", unique(condition)[1])) +
    ylab(paste0("Group ", unique(condition)[2]))
  if (!is.na(names)) plot1 <- plot1 + geom_text_repel(
    data = tail(data[order(data$meanB - data$meanA), ], names),
    aes(label = id),
    size = 3,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )

  return(plot1)
}
