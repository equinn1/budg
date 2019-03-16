library(brms)
ae = read.csv("notebooks/allexp.csv")
ae17 = na.omit(ae[ae$Year %in% c(2017,2016),])
rm(ae)
ae17$sub = as.factor(ae17$Sub)
ae17$loc = as.factor(ae17$Loc)
ae17$prog = as.factor(ae17$Prog)
ae17$func = as.factor(ae17$Func)
ae17$jc   = as.factor(ae17$JC)
ae17$obj  = as.factor(ae17$Obj)
ae17$district  = as.factor(ae17$District)
ae17$cata = interaction(ae17$prog,ae17$func,drop=TRUE)
ae17$catb = interaction(ae17$cata,ae17$jc,drop=TRUE)
ae17$catc = interaction(ae17$catb,ae17$obj,drop=TRUE)
ae17$absact = (ae17$Actual > 0.0)*ae17$Actual
ae17$logact = log(1+ae17$absact)
mu = mean(ae17$logact)
sigma = sd(ae17$logact)
ae17$logz = (ae17$logact-mu)/sigma
boxplot(ae17$logz)
summary(ae17$logz)

fit1 = brm(logz ~ (1|catc), data=ae17, chains=4, cores=4)

save(fit1,file="allride_fit1.Rdata")

