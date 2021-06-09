#' Calculate population growth rate (lambda) from a matrix population model.
#'
#' Based on code from Stevens 200x
#'
#' @param M Matrix population model
#'
#' @references Stevens, H. 200x.  A primer of ecology in R.
#'
#' @examples
#' M.test <- matrix(data = c(0.00,2.0, 0.25,0.9),nrow = 2, byrow = T)
#' calc_lam(M.test)
#'
#' @export

calc_lam <- function(M){
  eigs.A <- eigen(M)
  dom.pos <- which.max(eigs.A[["values"]])
  #lambda
  L1 <- Re(eigs.A[["values"]][dom.pos])

  return(L1)
}
