#rm(list=ls())

library(brms)

load("../../rs.Rdata")

rs = rs[rs$Actual > 1.0,]

rs$logact = log(rs$Actual+1)

objs = na.omit(rs[(rs$Year >= 2013)&(rs$Year <= 2017),
          c("Year","cat","District.Name","logact")])
summary(objs$logact)
logact_mean = mean(objs$logact,na.rm=TRUE)
logact_sd   = sd(objs$logact)
objs$logact_z = (objs$logact-logact_mean)/logact_sd
summary(objs$logact_z)
boxplot(objs$logact_z)
objs$year_z = (objs$Year - 2012)/2.5

get_prior(bf(logact_z ~ 1 + (1+year_z|cat / District.Name)),
                    data=objs)

prior <- c(prior(normal(0,2), class = sd, group=cat),
           prior(normal(0,0.2), class = sd, coef=year_z,
                 group=cat),
           prior(normal(0,3), class=sigma),
           prior(normal(0,2), class=Intercept)
           )

make_stancode(logact_z ~ 1 + (1+year_z|cat / District.Name),
              #control = list(max_treedepth = 12),
              #save_model='brmf2010_2014.txt',
              save_dso=TRUE,
              prior=prior,
              data=objs, chains=4, cores=4)

dl <- make_standata(logact_z ~ 1 + (1+year_z|cat / District.Name),
              #control = list(max_treedepth = 12),
              save_dso=TRUE,
              #algorithm="fullrank",
              data=objs, chains=4, cores=4)

rs2013_2017 = brm(logact_z ~ 1 + (1+year_z|cat / District.Name),
                    #control = list(max_treedepth = 12),
                    save_dso=TRUE,
                    prior=prior,
                    data=objs, chains=4, cores=4)

save(rs2013_2017,objs,file="rs2013_2017.Rdata")

library(rstan)

pd = extract(rs2010_2014$fit)

pd1 = extract(brmf1$fit)

for (i in 1:ncol(pd1$r_1_1)){
  print(i)
  print(mean(pd1$r_1_1[,i]))
  print(mean(pd1$r_1_2[,i]))
  #print(mean(pd1$r_2_sigma_1[,i]))
}
save(brmf1a,file="brmf1a.Rdata")
waic(brmf1,brmf1a)
