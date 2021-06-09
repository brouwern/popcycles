#' Calculate sensitivities and elasticities
#'
#' Returns a list with 2 items: S and E.
#'
#' Based on code from Stevens 200x
#'
#' @param M Matrix population model
#'
#' @references Stevens, H. 200x.  A primer of ecology in R.
#'
#' @examples
#' M.test <- matrix(data = c(0.00,2.0, 0.25,0.9),nrow = 2, byrow = T)
#' calc_S(M.test)
#'
#' @export

#### function to calc sensitivities
calc_S <- function(M){

  # prepliminaries
  ## Calculate lambda
  ## (needed for elasticities)
  eigs.A <- eigen(M)
  dom.pos <- which.max(eigs.A[["values"]])

  #lambda
  L1 <- Re(eigs.A[["values"]][dom.pos])


  #stable stage distribution
  ## TODO: Make sep. function
  w <- Re(eigs.A[["vectors"]][ , dom.pos])


  #rep value RV
  ## TODO: Make sep. function
  m <- eigen(t(M))
  v <- Re(m$vectors[ , which.max(Re(m$values))])

  RV <- v/v[1]

  #### Sensitivities

  # The numerator
  vw.s <- v%*%t(w)

  S <- vw.s/as.numeric(v%*%w)

  #### Elasticities
  ## E =
  E <- (M/L1)*S  #m1 is matrix, L1 is lambda, S is sensitivity matrix

  out <- list(S = S,
              E = E)
  return(out)
}

