
### average
df.ave <- data.frame(age.class = 1:6,
           surv.ann =  c(0.67, 0.74, 0.85, 0.88, 0.95, 0.50),
           fecunidty = c(0,0,0,0.26,0.36,0.15),
           stage.dur = c(1,1,5,2,21,5))

df.ave$gamma <- NA
for(i in 1:nrow(df.ave)){
  df.ave$gamma[i] <- agetostage(sig.i = df.ave[i,"surv.ann"],
                                T.i = df.ave[i,"stage.dur"])
}


df.ave$G.i <- df.ave$surv.ann*df.ave$gamma
df.ave$P.i <- df.ave$surv.ann*(1-df.ave$gamma)

m.ave <- diag(x = df.ave$P.i)
m.ave[lower.tri(m.ave)][c(1,6,10,13,15)] <- c(df.ave$G.i[-6])
m.ave[1,] <- df.ave$fecunidty

eigen(m.ave)



dom.pos <- which.max(eigen(hi.mat)[["values"]])
Re(eigen(hi.mat)[["values"]][dom.pos])







### high rainfall
df.high <- data.frame(age.class = 1:6,
                     surv.ann =  c(0.76, 0.78, 0.88, 0.95, 0.97, 0.52),
                     fecunidty = c(0,0,0.04,0.35,0.43,0.25),
                     stage.dur = c(1,1,5,2,21,5))

df.high$gamma <- NA
for(i in 1:nrow(df.high)){
  df.high$gamma[i] <- agetostage(sig.i = df.high[i,"surv.ann"],
                                T.i = df.high[i,"stage.dur"])
}


df.high$G.i <- df.high$surv.ann*df.high$gamma
df.high$P.i <- df.high$surv.ann*(1-df.high$gamma)

m.high <- diag(x = df.high$P.i)
m.high[lower.tri(m.high)][c(1,6,10,13,15)] <- c(df.high$G.i[-6])
m.high[1,] <- df.high$fecunidty

eigen(m.high)
