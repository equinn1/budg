#
rm(list=ls())

library(tidyr)


setwd('~/EG/school_committee/finance_subcommittee/budg')

budg = read.csv("../ucoa_history.csv")

str(budg)

budg$absact = (budg$actual>0.0)*budg$actual

budg$logexp = log(budg$absact+1)

str(budg)

boxplot(budg$logexp)

expn = budg[budg$loc != 99998,]

boxplot(expn$logexp)

summary(expn$logexp)
sd(expn$logexp)

library(rstan)

budg$t = as.numeric(budg$year - 2010)
budg$obj = as.factor(budg$obj)
budg$prog = as.factor(budg$prog)
budg$func = as.factor(budg$func)
budg$fund = as.factor(budg$fund)
budg$jc = as.factor(budg$jc)
budg$loc = as.factor(budg$loc)

#obj51110 = budg[budg$obj == 51110,]
obj51110 = budg

table(budg$year)

obj51110 = obj51110[(obj51110$year >= 2016)&(obj51110$year <= 2017),]

obj51110$func_jc = droplevels(interaction(obj51110$func,obj51110$jc,obj51110$loc))

boxplot(obj51110$logexp~obj51110$func_jc)

level = as.integer(obj51110$func_jc)
year = obj51110$year-2009
logexp = obj51110$logexp
N = nrow(obj51110)
n_levels = length(table(obj51110$func_jc))

rstan_options(auto_write = TRUE)              #use multiple cores
options(mc.cores = parallel::detectCores())   #if we have them

stanfit = stan("varying_slope.stan",chains=4)

summary(stanfit)

pd = extract(stanfit)

mean(pd$a)
mean(pd$b)


for(i in 1:383){
  fv = mean(exp(pd$a_level[,i] + 2*pd$b_level[,i]))
  print(fv)
}
fl11 = exp(pd$a_level[,1] + 1*pd$b_level[,1])
mean(fl11)
fl12 = exp(pd$a_level[,1] + 2*pd$b_level[,1])
mean(fl12)
mean(fl12)/mean(fl11)
fl41 = exp(pd$a_level[,4] + 1*pd$b_level[,4])
mean(fl41)
fl42 = exp(pd$a_level[,4] + 2*pd$b_level[,4])
mean(fl42)
mean(fl42)/mean(fl41)

