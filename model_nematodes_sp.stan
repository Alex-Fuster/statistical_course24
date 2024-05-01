data {
  int<lower=0> N;
  vector[N] parasitic_load; // response
  vector[N] island_age;
  vector[N] microhabitat;
  vector[N] phylodist;
}
parameters {
  real intercept;
  real slope;
  real<lower=0> sigma;
}
model {
  
  // priors
    intercept ~ normal(20, 5);
  slope ~ normal(0, 1);
  sigma ~ exponential(.7);
  
  //
  
  bill_dep ~ normal(intercept + slope * bill_len, sigma);

}

