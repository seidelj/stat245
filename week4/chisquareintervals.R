require("nleqslv")
v <- 10          # assign the degrees of freedom
CL<- 0.9                 # assign the desired confidence level for the C.I.

conf<- function(x) {
  y<- numeric(2)
  y[1]<- dgamma(x[1], shape=v/2, scale = 2)- dgamma(x[2], shape=v/2, scale = 2)
  y[2]<- CL-pgamma(x[2], shape=v/2, scale = 2) + pgamma(x[1], shape=v/2,  scale = 2)
  y
}

eL<- qgamma((1-CL)/2, shape=v/2, scale=2)
eU<- qgamma((1+CL)/2, shape=v/2, scale=2)
xstart<- c(eL,eU)      # the starting values are the equal-tails cut-offs
# eL may need to be reduced in value for very small degrees of freedom
nleqslv(xstart, conf, control=list(btol=.001))$x
qgamma(0.5, shape=v/2, scale = 2)   # check that the limits "straddle" the median
