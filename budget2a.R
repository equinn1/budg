library(tidyr)
library(rethinking)


budg = read.csv("../ucoa_history.csv")

budg$loc = as.factor(budg$loc)
budg$func = as.factor(budg$func)
budg$prog = as.factor(budg$prog)
budg$sub  = as.factor(budg$sub)
budg$fund   = as.factor(budg$fund)
budg$jc   = as.factor(budg$jc)
budg$obj  = as.factor(budg$obj)

neg <- budg[budg$actual < 0.0,]

budg$absact = (budg$actual>0.0)*budg$actual

budg$logact = log(budg$absact+1)

str(budg)

expn = budg[budg$loc != 99998,]
expn$loc = as.factor(expn$loc)
expn$obj = as.factor(expn$obj) 

boxplot(expn$absact)

boxplot(expn$logact)

summary(expn)

f111 = expn[expn$func==111,]
f111a = f111[f111$obj==51110,]
f111a$rskey = droplevels(interaction(f111a$loc,f111a$sub,f111a$prog,f111a$jc))
f111a$rskeyi = as.integer(f111a$rskey)
f111b = f111a[c('rskeyi','logact','year')]
f111c = f111b[f111b$logact > 0.0,]

summary(f111c)

table(f111c$rskeyi)

m13.2 <- ulam(
  alist(
    logact ~ dnorm( u , sigma2 ) ,
    u <- a[rskeyi],
    a[rskeyi] ~ dnorm( a_bar , sigma ) ,
    a_bar ~ dnorm( 0 , 1.5 ) ,
    sigma ~ dexp( 1 ),
    sigma2 ~ dexp(1)
  ), data=f111b , chains=4 , log_lik=FALSE)
summary(m13.2)

