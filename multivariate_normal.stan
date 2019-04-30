data{
    int<lower=0> NvarsX;   // number variables
    int<lower=0> N;        // Total number of rows
    vector[NvarsX] x [N];  // data for independent vars
}

parameters{
    cholesky_factor_corr[NvarsX] L_Omega; // L form of correlation matrix
    vector<lower=0>[NvarsX] L_sigma;      // Vector of scales for covariance matrix
    vector[NvarsX] mu;              // vector of mean value for each variable
}

transformed parameters{
    matrix[NvarsX, NvarsX] L_Sigma;
    L_Sigma = diag_pre_multiply(L_sigma, L_Omega);    // L form of Sigma covariance matrix
}

model{
    //Priors
    L_Omega ~ lkj_corr_cholesky(1);
    L_sigma ~ normal(0, 5);

    //likelihood
    x ~ multi_normal_cholesky(mu, L_Sigma);
}

generated quantities{
    matrix[NvarsX, NvarsX] Omega;   // Square form of correlation matrix
    matrix[NvarsX, NvarsX] Sigma;   // Square form of covariance matrix
    
    // Generate correlation matrix
    Omega = multiply_lower_tri_self_transpose(L_Omega);     
    Sigma = quad_form_diag(L_Omega, L_sigma);               
}