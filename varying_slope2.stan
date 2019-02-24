data{
    int N;
    int n_levels;
    vector[N] logexp;
    int year[N];
    int level[N];
}
parameters{
    vector[n_levels] b_level;
    vector[n_levels] a_level;
    vector<lower=0>[n_levels] sigma;
    real a;
    real b;
    vector<lower=0>[2] sigma_level;
    //real<lower=0> sigma;
    corr_matrix[2] Rho;
}
model{
    vector[N] mu;
    Rho ~ lkj_corr( 2 );
    sigma ~ exponential( 1 );
    sigma_level ~ exponential( 1 );
    b ~ normal( 0 , 0.5 );
    a ~ normal( 5 , 3 );
    {
    vector[2] YY[n_levels];
    vector[2] MU;
    MU = [ a , b ]';
    for ( j in 1:n_levels ) {
      YY[j] = [ a_level[j] , b_level[j] ]';
      sigma[j] ~ exponential(1);
      }
    YY ~ multi_normal( MU , quad_form_diag(Rho , sigma_level) );
    }
    for ( i in 1:N ) {
        mu[i] = a_level[level[i]] + b_level[level[i]] * year[i];
    }
    logexp ~ normal( mu , sigma );
  }
}
generated quantities{
    vector[N] log_lik;
    vector[N] mu;
    vector[N] sig;
    for ( i in 1:N ) {
        mu[i] = a_level[level[i]] + b_level[level[i]] * year[i];
    }
    for ( i in 1:N ) log_lik[i] = normal_lpdf( logexp[i] | mu[i] , sig[i] );
}