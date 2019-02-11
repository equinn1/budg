#
library(tidyr)

setwd('~/EG/school_committee/finance_subcommittee')

str(budg)

budg$absact = (budg$actual>0.0)*budg$actual

budg$logact = log(budg$absact+1)

str(budg)

boxplot(budg$logact)

expn = budg[budg$loc != 99998,]

boxplot(expn$logact)

summary(expn$logact)

library(rethinking)

sd(expn$logact)