#' Determine if a matrix is ergodic  By Iain Stott
#' "This function will test the ergodicity of a matrix based upon whether its dominant left eigenvector is positive or not.  A reducible-ergodic matrix has a positive dominant left eigenvector, where a reducible-nonergodic matrix will contain zeroes
#' in its dominant left eigenvector.  An irreducible matrix is always ergodic.
#'
#' "NOTE: this function can prove problematic for imprimitive, irreducible matrices or
#' reducible matrices with imprimitive, irreducible blocks on the diagonal.  The
#' 'dominant' left eigenvector chosen by R is that with the largest absolute value
#' (including both real and imaginary components), so that sometimes it chooses a dominant
#' with imaginary components.  In addition, where a reducible matrix in block-permuted form
#' contains one or more irreducible, imprimitive blocks on the diagonal, values of zero in
#' the dominant left eigenvector may actually be calculated by R as small numbers, approximate
#' to zero.  Hence, for such matrices, R may return a spurious result of the matrix being ergodic
#' when in fact it is nonergodic.  For this reason, the 'dominant' left eigenvector is also returned
#' by the function so that it may be checked by eye: if it contains non-zero imaginary components,
#' or if it contains small numbers that may approximate to zero then further diagnosis may be
#' required.  The 'blockmatrix' and 'is.primitive' functions can help to
#' diagnose whether diagonal blocks are imprimitive or not.
#'
#' @param M Matrix population model
#'
#' @references Stott et al. 2010. On reducibility and ergodicity of population projection matrix model.  Methods in Ecology and Evoltuion.
#'
#' @examples
#'
#' # A good 2 x 2 matrix
#' M.good <- matrix(data = c(0.00, 2.0, 0.25,0.9),nrow = 2, byrow = T)
#' is_irreducible(M.good)
#' is_ergodic(M.good)
#'
#' M.bad <- matrix(data = c(0.00, 2.0, 0.0,
#'                          0.25, 0.9, 0.0,
#'                          0.00, 0.1, 0.9),
#'                          nrow = 3, byrow = T)
#' # A "bad" 3 x 3 matrix - irreproducible because it has a post-reproductive stage
#' is_irreducible(M.bad)
#' is_ergodic(M.bad)
#'
#' @export

is_ergodic <- function(M){

  #create an object 'order' that is equal to the dimension of the matrix
  ord <- sqrt(length(M))

  #create an object 'xt' that is the transpose of M
  xt <- t(M)

   #create an object 'lefteigvec' that is the dominant right eigenvector of xt, hence the dominant
  #left eigenvector of x
  lefteigvec <- as.matrix(eigen(xt)$vectors[,1])

  #create an object 'abslefteigvec' that is the modulus of the dominant left eigenvector
  abslefteigvec <- as.matrix(abs(lefteigvec))

  #create an empty object 'ergodic'
  ergodic=numeric(1)

  #if the minimum absolute value of the dominant left eigenvector is >0 then the matrix is
  #ergodic, otherwise it is nonergodic
  if(min(abslefteigvec)>0) ergodic=noquote("TRUE: Ergodic")
    else(ergodic=noquote("FALSE: Nonergodic (that's bad)"))

  #return 'ergodic' and 'lefteigvec'.
  return(list(ergodic=ergodic,lefteigvec=lefteigvec))}
