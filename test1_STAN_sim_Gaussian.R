# prompt:
# after the prompts of test_Stan2, ask "Do the same but for a Gaussian with mean of 0.5 and variance of 0.1"
# alright it's confusing variance and std so I'll do it myself
# new functionality I will try now is to simulate from the STAN code itself

# Load required libraries
library(rstan)
library(ggplot2)

# Set a seed for reproducibility
set.seed(123)

# Specify the Stan model
stan_code <- '
  data {
    int<lower=0> N;          // Number of data points
    real y[N];               // Observed data
    int<lower = 0, upper = 1> run_estimation; // a switch to evaluate the likelihood
  }
  parameters {
    real mu;                 // Mean parameter
    real<lower=0> sigma;     // Standard deviation parameter
  }
  model {
    mu ~ normal(0, 1);       // Prior for mean
    sigma ~ cauchy(0, 1);    // Prior for standard deviation
    if(run_estimation==1){
      y ~ normal(mu, sigma);   // Likelihood
    }
  }
  generated quantities{
    vector[N] y_sim;
    for(i in 1:N) {
      y_sim[i] = normal_rng(mu, sigma); //copy the likelihood line, but!! need to change to _rng for generating data
    }
  }
'
# Compile the Stan model
compiled_model <- stan_model(model_code = stan_code)

#=============do the simulations===============

# Simulate data from a Gaussian distribution; sample values from prior?
n_sim <- 1000
n_draws_from_prior <- 5

sim_out <- sampling(compiled_model, data = list(N = n_sim, 
                                                y = sample(1:2, n_sim, replace = T), 
                                                run_estimation = 0),
                    chain=3,iter = 100, warmup = 60)
sim_params  <- sim_out %>% 
  as.data.frame %>% 
  dplyr::select(mu, sigma)
sim_results <- sim_out %>% 
  as.data.frame %>% 
  dplyr::select(contains("y_sim"))
#okay, not sure what is determining the number of draws this sampling() will get from the prior??
#Hmm I get it now: iter = 100, but we have 4 chains; every chain will be used, but burn-in will not be used
#so, we get n_chain*(n_iter-n_warm_up) simulations

#now see if the mean & sd of the simulated data are like the parameters that generated them:
sim_params$mu_sim <- apply(sim_results, 1, mean)
sim_params$sigma_sim <- apply(sim_results, 1, sd)

ggarrange(
  ggplot(sim_params,aes(mu,mu_sim))+geom_point()+theme_classic(),
  ggplot(sim_params,aes(sigma,sigma_sim))+geom_point()+theme_classic()
)


