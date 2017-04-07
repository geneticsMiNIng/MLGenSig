#' @title genereg_vs_met
#'
#' @description Function \code{genereg_vs_met} ...
#'
#' @param data data frame containing values of methylation: columns coresponds to CpG islands, rows to samples.
#' @param gen vector of levels coresponding to order of samples in data.
#'
#' @return A plot.
#'
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_line
#'@importFrom ggplot2 theme_bw
#'
#' @export

genereg_vs_met <-function(data, gen){
  CpG <- CpG_mean(data, gen)
  plot <- ggplot(CpG, aes(Name, mean, group=1)) +
            geom_point(size=4)+
            geom_line()+
            theme_bw()
  return(plot)
}

