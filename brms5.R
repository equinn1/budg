library(brms)

rm(list=ls())


pd=extract_draws(fm3)

summary(fm3)

ixx = seq(2,9242/2,2)
ixi=ixx-1

ip = pd$dpars$mu$re$r$lvl[1,ixi]
xp = pd$dpars$mu$re$r$lvl[1,ixx]

sum(exp(ip+xp))
sum(exp(ip+3*xp))
