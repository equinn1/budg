#rm(list=ls())

library(brms)

budg =read.csv("../ucoa_history.csv")

budg$absact = (budg$actual>0.0)*budg$actual

budg$logact = log(budg$absact+1)

boxplot(budg$logact)

expn = budg[budg$loc != 99998,]

boxplot(expn$logact)

#save(budg,file="../budg.Rdata")

expn$obj = as.factor(expn$obj)
expn$prog = as.factor(expn$prog)
expn$func = as.factor(expn$func)
expn$fund = as.factor(expn$fund)
expn$jc = as.factor(expn$jc)
expn$loc = as.factor(expn$loc)

objs = expn
objs = objs[(objs$year >= 2010)&(objs$year <= 2014),]
logact_mean = mean(objs$logact)
logact_sd   = sd(objs$logact)
objs$logact_z = (objs$logact-logact_mean)/logact_sd
boxplot(objs$logact_z)
objs$year_z = (objs$year - 2012)/2.5
objs$key = droplevels(objs$key)

get_prior(bf(logact_z ~ (1 + year_z) + (1|key)),
                    data=objs)

prior <- c(prior(normal(0,3), class=b),
           prior(normal(0,2), class = sd, group=key),
           prior(normal(0,0.2), class = b, coef=year_z),
           prior(normal(0,3), class=sigma),
           prior(normal(0,2), class=Intercept)
           )

make_stancode(logact_z ~ 1 + year_z + (1|key),
              prior=prior,
              #control = list(max_treedepth = 12),
              save_model='brmf2010_2014.txt',
              save_dso=TRUE,
              data=objs, chains=4, cores=4)

dl <- make_standata(logact_z ~ 0 + (0+year_z|key),
              control = list(max_treedepth = 12),
              save_model='brmf2010_2018.txt',
              save_dso=TRUE,
              algorithm="fullrank",
              data=objs, chains=4, cores=4)

brmf2010_2014 = brm(logact_z ~ 1 + (0+year_z|key),
                    #control = list(max_treedepth = 12),
                    save_model='brmf2010_2014.txt',
                    save_dso=TRUE,
                    prior=prior,
                    data=objs, chains=4, cores=4)

save(brmf2010_2014,objs,file="brmf2010_2014.Rdata")

pd1 = extract(brmf1$fit)

for (i in 1:ncol(pd1$r_1_1)){
  print(i)
  print(mean(pd1$r_1_1[,i]))
  print(mean(pd1$r_1_2[,i]))
  #print(mean(pd1$r_2_sigma_1[,i]))
}
save(brmf1a,file="brmf1a.Rdata")
waic(brmf1,brmf1a)
