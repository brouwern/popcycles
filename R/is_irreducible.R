#' Determine if a matrix is irreducible (good) or reducible (can be bed).  By Iain Stott
#'
#' This function tests whether a matrix is reducible or irreducible.
#' IT WILL ONLY RELIABLY WORK FOR NONNEGATIVE MATRICES (e.g. PPMs).
#' It works on the basis that for any matrix A, (I+A)^(s-1) where I
#' is the identity matrix and s is equal to the order of the matrix
#' is positive (Caswell 2001).  The function returns the condition
#' "TRUE: Irreducible" or "FALSE: Reducible".
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
#'
#' M.bad <- matrix(data = c(0.00, 2.0, 0.0,
#'                          0.25, 0.9, 0.0,
#'                          0.00, 0.1, 0.9),
#'                          nrow = 3, byrow = T)
#' # A "bad" 3 x 3 matrix, with a post-reproductive stage
#' is_irreducible(M.bad)
#'
#' @export

#set up function and parameters
is_irreducible <- function(M){

  #create an object 'ord' that is equal to the dimension of M.
  ord <- sqrt(length(M))

  #create an ident matrix of the same size as M
  ident <- diag(ord)

  #create an object 'identityplusx'=I+A
  identityplusx <- ident+M

  #create a character string for multiplication of I+A and repeat it s-2 times
  string <- rep("identityplusx%*%",ord-2)

  #end the character string so that it now repeats multiplication of I+A s-1 times
  string <- c(string,"identityplusx")

  #collapse the character and evaluate it, omitting quotation marks
  string <- paste(string,collapse="")
  powermatrix <- eval(parse(text=noquote(string)))

  #create an object 'minval' that is equal to the smallest entry of the matrix
  minval <- min(powermatrix)

  #if this minimum is greater than 0 (i.e. the matrix is positive), then return
  #"TRUE: Irreducible", if it is not greater than 0 (i.e. the matrix is not positive)
  #then return "FALSE: Reducible", omitting quotation marks.
  if(minval>0)
    return(noquote("TRUE: Irreducible.  (Good!)"))
    else(noquote("FALSE: Reducible (Possibly bad...)"))
  }
