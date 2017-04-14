# Simulation to compare 
# CI approximation (binomial)
# Wald, Wilson, Arcsin transformation


nsims = 100
n=150
p=.1
alpha=.05
z = qnorm(1-alpha/2)

samples = rbinom(nsims, size=n, p=p)
p.hats = samples/n

lower.wald = p.hats - qnorm(1-alpha/2)*sqrt( (p.hats*(1-p.hats)) /n)
upper.wald = p.hats + qnorm(1-alpha/2)*sqrt( (p.hats*(1-p.hats)) /n)

lower.wilson = (p.hats + (z^2)/(2*n))/(1+(z^2)/n) - sqrt( (p.hats*(1-p.hats))/n*z^2 + (z^4)/(4*n^2))/(1 + (z^2)/n)
upper.wilson = (p.hats + (z^2)/(2*n))/(1+(z^2)/n) + sqrt( (p.hats*(1-p.hats))/n*z^2 + (z^4)/(4*n^2))/(1 + (z^2)/n)

lower.arcsin = sin(asin(sqrt(p.hats)) - z/(2*sqrt(n)))^2
upper.arcsin = sin(asin(sqrt(p.hats)) + z/(2*sqrt(n)))^2

mean(lower.wald <= p & upper.wald >= p)
mean(lower.wilson <= p & upper.wilson >= p)
mean(lower.arcsin <= p & upper.arcsin >= p)
