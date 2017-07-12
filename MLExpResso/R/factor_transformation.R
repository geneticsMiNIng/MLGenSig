#'Function converts vector of class factor to numeric
#'
#'@param x factor vector
#'


factor_transformation <- function(x){
  y <- as.numeric(levels(x))[x]
  return(y)
}