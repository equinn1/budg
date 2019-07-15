data{
  int<lower=0>         n;             // number of observed step values
  int<lower=0>         stepcount;     // number of entries in salary matrix
  int<lower=0,upper=stepcount> y[n];
  vector<lower=0,upper=1>[stepcount] priorp; 
}
parameters{
   simplex[stepcount] p;
}
model{
  p ~ dirichlet(priorp);
  y ~ categorical(p);
}
