data {
  int<lower=0> Ncorrect;    // number of correct obs 
  int<lower=0> Nwrong;          // number of obs 
  real y[Ncorrect];                // RTs 
  real cutoff[Nwrong];  
}

parameters {
  real mu; 
  real<lower=0> sigma;
  real<lower=0, upper=10> lambda;
  real<lower=0, upper=1> p;          
}

model {
  mu ~ normal(0,10);
  sigma ~ normal(0,0.4);
  lambda ~ uniform(0,10);
  p ~ uniform(0,1);

  target += Ncorrect*log(p) + exp_mod_normal_lpdf(y | mu,sigma,lambda);
  for (n in 1:Nwrong){
    target += log((1-p) + p*(1-exp_mod_normal_cdf(cutoff[n], mu,sigma,lambda)));
  }
}
