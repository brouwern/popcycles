stage_duration_gamma <- function(sig.i,T.i,lambda=1){
  lambda*( (sig.i/lambda)^T.i -  (sig.i/lambda)^(T.i-1) ) / ((sig.i/lambda)^T.i - 1)
}
