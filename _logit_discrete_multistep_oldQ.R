logit_discrete_multistep <-function(alpha = 0.01,
                                    r.d = 1,
                                    N0 = 2,
                                    time.steps = 15,
                                    add.params = TRUE,
                                    ...){
  #expand time to vector
  N.t <- rep(NA, time.steps)
  N.t <- c(N0,N.t)


  for(t in 1:time.steps){
    N.t[t+1] <- logit_discrete_1step(alpha,r.d,N.t[t])

  }

  time <- c(0,1:time.steps)

  df <- data.frame(time = time,
                   N.t = N.t)

  if(add.params == TRUE){
    df <- data.frame(df,
                     alpha,
                     N0,
                     time.steps)
  }

  return(df)
}
