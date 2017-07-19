#'@title calculate_condition_means
#'
#'@description Function \code{calculate_condition_means} produces a data frame with columns correspond to means by condition.
#'
#'@param data datset
#'@param condition conditions for means
#'
#'@return dataframe.
#'

calculate_condition_means <- function(data, condition){
    means <- aggregate(data, list(condition), mean, na.rm=TRUE)
    means <- t(means)
    colnames(means) <- paste0("mean_",means[1,])
    means <- as.data.frame(means[-1,])
    means$mean <- colMeans(data, na.rm=TRUE)
    means$id <- rownames(means)
    rownames(means) <- NULL
    return(means)
  }
