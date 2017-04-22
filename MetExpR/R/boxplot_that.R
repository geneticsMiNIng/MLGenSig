#'@title boxplot_that
#'
#'@description Function \code{boxplot_that} generates a boxplot of values from choosen data frame column.
#'
#'@param data data frame containing interesing values.
#'@param column string containing name of column with values for boxplot.
#'@param condition vector of length equal to numer of rows of data,
#'\code{condition} should contains names of groups corresponding to rows.
#'
#'@return Object of class ggplot containing boxplot for values from choosen column.
#'
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_boxplot
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 theme
#'@importFrom ggplot2 ggtitle
#'@importFrom ggplot2 element_blank
#'@importFrom ggplot2 xlab
#'
#'@export

boxplot_that <- function(data, column, condition=""){
  x <- data[,column]
  x<-as.data.frame(x)

  x$condition <- condition
  colnames(x)[1] <- "values"
  x$column <- paste0(column)
  colnames(x)[2] <- paste0(column)
  
  plot <- ggplot(x,aes(condition,values,col=condition))+
    geom_boxplot()+
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.title.y =element_blank(),
          legend.position="none")+
    ggtitle(paste(column))+
    xlab(condition)

  return(plot)
}
