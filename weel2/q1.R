# Simulation to compare CI 
# Wilson's method vs regular

nsims = 1000
n=30
lam=1
alpha = .05

lam.hats=rep(NA, nsims)
for (i in 1:nsims){
  samples = rpois(n, lam)
  lam.hats[i] = mean(samples)
}

lower.regular = lam.hats - qnorm(1-alpha/2)*sqrt(lam.hats/n)
upper.regular = lam.hats + qnorm(1-alpha/2)*sqrt(lam.hats/n)

lower.wilson =( 2*n*lam.hats + qnorm(1-alpha/2)^2 )/(2*n) - sqrt((2*n*lam.hats+qnorm(1-alpha/2)^2)^2-4*n^2*lam.hats^2)/ (2*n)
upper.wilson =( 2*n*lam.hats + qnorm(1-alpha/2)^2 )/(2*n) + sqrt((2*n*lam.hats+qnorm(1-alpha/2)^2)^2-4*(n*lam.hats)^2)/ (2*n)

mean(lower.regular <= lam & upper.regular>= lam)
mean(lower.wilson <= lam & upper.wilson>= lam)

