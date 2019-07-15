library(MCMCpack)
library(e1071)
library(rstan)
library(LaplacesDemon)

#rm(list=ls())

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

counts = read.csv("step_counts.csv",header=FALSE)   #read step history

cm = as.integer(data.matrix(counts))                #convert to integer vector
n = sum(cm)                                         #number of observed values

y=rep(1:length(cm),cm)                              #vector of observed steps
table(y)                                        

#stan data list
#  prior probabilities are equal for all steps:  1/stepcount

stepcount = length(cm)                             #number of entries in salary table

priorp = rep(1/stepcount,stepcount)                #uninformative prior - equal prob

dl = list(n=n,stepcount=stepcount,y=y,priorp=priorp)  #Stan data list

ss = stan("step_counts.stan",data=dl,                              #run stan
          control = list(adapt_delta = 0.99,max_treedepth = 12))

summary(ss)                           #summarize the output from Stan
pd = extract(ss)                      #extract the posterior draw
str(pd)

library(shinystan)                    #use shinystan to explore the results
launch_shinystan(ss)
  