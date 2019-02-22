## brms example from vignette
library(lme4)
library(MASS)
library(brms)

rm(list=ls())

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

credits = expn[expn$actual < 0.0,]
table(credits$year)

boxplot(expn$absact)
boxplot(expn$logact)
boxplot(expn$logact~expn$obj)

table(expn$obj)

df = expn[(expn$x>=4)&(expn$x<=99)&(expn$obj %in% c(51110, 
    52102,52103,52121,52203,52301,52302,56101,52208,52213,
    52501,52710,53301,53303,53503,55111,56401,57305,58102)),]
df$x = df$x-4
table(df$x)
df$lvl = as.integer(df$key)
#levels(df$key)
table(df$obj)


str(df)

fm2 <- lmer(logact ~ x + (x | lvl), df)
summary(fm2)

fm3 <- brm(logact ~ x + (x | lvl), df, chains=4, cores=4,iter=4000)
summary(fm3)

#save(fm3,file="fm3_test5.Rdata")
str(fm3)
