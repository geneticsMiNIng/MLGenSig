#'@title Boxplot for expression in groups
#'
#'@description Function \code{group_boxplot} generates a boxplot of values from choosen data frame column with division in groups.
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

group_boxplot <- function(data, column, condition="", sqrt.trans=FALSE){
  if(is.vector(data)) {
    dt <- as.data.frame(data)
    colnames(dt) <- column
  }else{
    dt <- data[,column]
    dt<-as.data.frame(dt)
  }
  dt$condition <- condition
  colnames(dt)[1] <- "values"
  dt$column <- paste0(column)
  colnames(dt)[2] <- paste0(column)

  plot <- ggplot(dt,aes(condition,values,col=condition))+
    geom_boxplot(outlier.size = 0.5)+
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.title.y =element_blank(),
          legend.position="none",
          panel.border = element_blank())+
    xlab(condition)+
    scale_color_manual(values=c("#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc"))

  if(sqrt.trans==TRUE) plot <- plot + coord_trans(y="sqrt")

  return(plot)
}
