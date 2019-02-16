## brms example from vignette
library(lme4)
library(MASS)
library(brms)

budg = read.csv("../ucoa_history.csv")
str(budg)

budg$absact = (budg$actual>0.0)*budg$actual

budg$logact = log(budg$absact+1)
budg$x = budg$year - 2009

str(budg)

expn = budg[budg$loc != 99998,]
expn$Loc = as.factor(expn$loc)
levels(expn$Loc)

str(expn)

summary(expn)

boxplot(expn$absact)
boxplot(expn$logact)
boxplot(expn$logact~expn$obj)

table(expn$obj)

df = expn[(expn$x>=4)&(expn$x<=5),]
df$x = df$x-4
table(df$x)
df$lvl = as.integer(df$key)
levels(df$key)
table(df$lvl)


str(df)

fm2 <- lmer(logact ~ x + (x | lvl), df)
summary(fm2)

fm3 <- brm(logact ~ x + (x | lvl), df, chains=1, cores=1,iter=4000)
summary(fm3)

#save(fm3,file="fm3_test2.Rdata")
str(fm3)
