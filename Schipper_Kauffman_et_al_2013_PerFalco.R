#Modelling interactions of toxicants and density dependence in wildlife populations
#Schipper, Kauffman et al Journal of Applied Ecology 2013, 50, 1469-1478

S.jw <- 0.38
S.jc <- 0.24
S.nb <- 0.86
S.b  <- 0.86
F.r  <- 1.75

x0   <- 2.36
x1   <- -0.014

a.dde    <- -0.12
b.dde    <-  238
#a.pbde   <-
#b.pbde


P.b.t <- 1


fx.make.per.mat <- function(S.jw,S.jc,
                            S.nb,
                            F.r,
                            P.b.t){
  per.mat <- c(0.00, 0.00, S.nb*F.r*P.b.t,  S.b*F.r,
               0.00, 0.00, 0.00,            0.00,
               S.jw, S.jc, S.nb*(1-P.b.t),  0.00,
               0.00, 0.00, S.nb*P.b.t,      S.b)
  
  
  per.mat <- matrix(per.mat,nrow = 4, byrow = T)
  
  return(per.mat)
}

N.per <- c(10,10,10,10)
names(N.per) <- c("jw","jc","nb","b")
fx.P.b.t <- function(x0, x1, N.per){
  exp(x0 + x1*N.per["b"])/(1 + exp(x0 + x1*N.per["b"]))
}

N.per <- 1:100
names(N.per) <- "b"

plot(fx.P.b.t(x0,x1,N.per)

N.per.vec <- 1:500
plot(exp(x0 + x1*N.per.vec)/(1 + exp(x0 + x1*N.per.vec)) ~ N.per.vec)
abline(v = 308, col = 2)
