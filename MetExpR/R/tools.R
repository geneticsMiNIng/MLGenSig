#'@title Scales for volcano plots
#'
#'@description Function \code{tools} crate a logarithm transformation for y-axis..
#'
#'@param base d
#'


reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv,
            log_breaks(base = base),
            domain = c(1e-100, Inf))
}
