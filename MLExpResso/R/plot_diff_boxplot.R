#'@title Boxplot for expression in groups
#'
#'@description Function \code{plot_diff_boxplot} generates a boxplot of values from choosen data frame column with division in groups.
#'
#'@param data data frame containing interesing values.
#'@param column string containing name of column with values for boxplot.
#'@param condition vector of length equal to numer of rows of data,
#'\code{condition} should contains names of groups corresponding to rows.
#'@param sqrt.trans root square transformation
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
#'@importFrom ggplot2 coord_trans
#'
#'@export

plot_diff_boxplot <- function(data, column, condition="", sqrt.trans=FALSE){
  values <- NULL

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

  plot <- ggplot(dt,aes(condition,y=values,col=condition))+
    geom_boxplot(outlier.size = 0.5)+
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.title.y =element_blank(),
          axis.text.x = element_text(size=15,angle = 0, hjust = 0.5),
          axis.text.y = element_text(size=15),
          legend.position="none",
          panel.border = element_blank())+
    xlab(condition)+
    scale_color_manual(values=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf")
)

  if(sqrt.trans==TRUE) plot <- plot + coord_trans(y="sqrt")

  return(plot)
}
