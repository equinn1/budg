rm(list=ls())

library(tidyr)

budg =read.csv("../ucoa_history.csv")

str(budg)

budg$absact = (budg$actual>0.0)*budg$actual

budg$logact = log(budg$absact+1)

str(budg)

boxplot(budg$logact)

expn = budg[budg$loc != 99998,]

boxplot(expn$logact)

summary(expn$logact)
sd(expn$logact)

library(rstan)

budg$t = as.numeric(budg$year - 2010)
budg$obj = as.factor(budg$obj)
budg$prog = as.factor(budg$prog)
budg$func = as.factor(budg$func)
budg$fund = as.factor(budg$fund)
budg$jc = as.factor(budg$jc)
budg$loc = as.factor(budg$loc)

table(budg$obj)

obj51110 = budg[budg$obj %in% c(51110,52301,52302),]
#obj51110 = budg

obj51110 = obj51110[(obj51110$year >= 2012)&(obj51110$year <= 2016),]

obj51110 = obj51110[obj51110$actual > 50.0,]

obj51110$func_jc = droplevels(interaction(obj51110$func,obj51110$jc,obj51110$loc))

level    = as.integer(obj51110$func_jc)
yr       = obj51110$year-2014
mean_logact = mean(obj51110$logact)
obj51110$clogact   = obj51110$logact-mean_logact   #centered
clogact   = obj51110$clogact
N        = nrow(obj51110)
n_levels = length(table(obj51110$func_jc))
boxplot(obj51110$clogact~obj51110$func_jc)

rstan_options(auto_write = TRUE)              #use multiple cores
options(mc.cores = parallel::detectCores())   #if we have them

stanfit = stan("varying_slope3.stan",chains=4,iter=2000,control = list(adapt_delta = 0.95,max_treedepth = 12))

summary(stanfit)

pd = extract(stanfit)

mean(pd$a)
mean(pd$b)


means = vector('numeric')
sds   = vector('numeric')

for(i in 1:n_levels){
  means = c(means,mean(exp(pd$a_level[,i] + 2*pd$b_level[,i]+mean(obj51110$logact))))
  sds = c(sds,pd$sig[i])
}
sum(means)
boxplot(sds)


