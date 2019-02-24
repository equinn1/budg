data{
    int N;
    int n_levels;
    vector[N] clogact;
    int yr[N];
    int level[N];
}
parameters{
    vector[n_levels] b_level;
    vector[n_levels] a_level;
    real a;
    real b;
    vector<lower=0>[2] sigma_level;
    vector<lower=0>[n_levels] sig;
    real<lower=0> sig_a;
    corr_matrix[2] Rho;
}
model{
    vector[N] mu;
    vector[N] sigma;
    Rho ~ lkj_corr( 2 );
    sig_a ~ uniform( 0 , 2 );
    sig ~ exponential( sig_a );
    for ( i in 1:N ) {
        sigma[i] = sig[level[i]];
    }
    sigma_level ~ exponential( 1 );
    b ~ normal( -1 , 0.5 );
    a ~ normal( 5 , 2 );
    {
    vector[2] YY[n_levels];
    vector[2] MU;
    MU = [ a , b ]';
    for ( j in 1:n_levels ) YY[j] = [ a_level[j] , b_level[j] ]';
    YY ~ multi_normal( MU , quad_form_diag(Rho , sigma_level) );
    }
    for ( i in 1:N ) {
        mu[i] = a_level[level[i]] + b_level[level[i]] * yr[i];
        sigma[i] = sig[level[i]];
    }
    clogact ~ normal( mu , sigma );
}
generated quantities{
    vector[N] log_lik;
    vector[N] mu;
    vector[N] sigma;
    for ( i in 1:N ) {
        mu[i] = a_level[level[i]] + b_level[level[i]] * yr[i];
        sigma[i] = sig[level[i]];
    }
    for ( i in 1:N ) log_lik[i] = normal_lpdf( clogact[i] | mu[i] , sigma[i] );
}
