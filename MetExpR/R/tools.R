#'@title Visualise the p-values of expression and methylation for genes.
#'
#'@description Function \code{volcano_plot} draws a plot with p-values and fold logarithm from methylation or expression when we use the t-test.
#'
#'
#'@param base data.fra
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
#'@importFrom scales trans_new
#'@importFrom ggthemes extended_range_breaks
#'@importFrom scales math_format
#'@importFrom scales log_breaks
#'@export


reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv,
            log_breaks(base = base),
            domain = c(1e-100, Inf))
}
