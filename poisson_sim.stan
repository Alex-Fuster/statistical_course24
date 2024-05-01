//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// Simulation model in Stan

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> n_people;
  
}


generated quantities {
  
  
  real<lower=0> avg_birds_per_person;
  
  // what is this thing, [size] type name
  
  array[n_people] int<lower=0> bird_count;
  
  
  avg_birds_per_person = uniform_rng(0,60);
  
  for (i in 1:n_people){
    
    bird_count[i] = poisson_rng(avg_birds_per_person);
    
  }
  
}
