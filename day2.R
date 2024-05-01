


library(tidyverse)

set.seed(525600)

simulate_some_birds <- function() {
  lambda <- runif(1, min = 0, max = 60)
  data.frame(obs = rpois(23, lambda = lambda))
}

rep_list <- replicate(12, simulate_some_birds())

tibble::tibble(simulation = 1:12,
               obs = rep_list) |> 
  unnest(cols = "obs") |> 
  ggplot(aes(x = obs)) + 
  geom_histogram(bins = 28) + 
  facet_wrap(~simulation) + 
  theme_bw() + 
  labs(x = "Number of birds observed per person")




##################

library(cmdstanr)

poisson_stim = cmdstan_model(stan_file = "poisson_sim.stan",
                             pedantic = TRUE)

# 2 make data

data_list <- list(n_people = 22)
  
  
# 3 sample

poisson_sim_sample = poisson_stim$sample(data = data_list,
                    fixed_param = TRUE)


library(tidybayes)
pois_sim <- tidybayes::spread_draws(poisson_sim_sample, 
                                    avg_birds_per_person,
                                    bird_count[],
                                    ndraws = 20,
                                    seed = 525600)


pois_sim |> 
  ggplot(aes(x = bird_count)) + 
  geom_histogram(fill = "orange") + 
  geom_vline(aes(xintercept = avg_birds_per_person), col = "darkgreen", lwd = 1) + 
  facet_wrap(~.draw) + 
  theme_bw()
  

library(readxl)

data_kraemer = read_excel("nematodes_data.xlsx", sheet = "data_kraemer")
df = data.frame(unique(data_kraemer$species),unique(data_kraemer$species))
write.csv(df, "df_sppnames_john.csv")


#####################################



bird_count = rpois(30,6)

mean(bird_count)

bird_data_list <- list(bird_count_observed = bird_count,
                       n_people = length(bird_count))


poisson_stim = cmdstan_model(stan_file = "stan_model.stan",
                             pedantic = TRUE)



poisson_sim_sample = poisson_stim$sample(data = bird_data_list,
                                           fixed_param = TRUE)



## Extract draws from the model object
poisson_model_draws <- poisson_sim_sample$draws()

bayesplot::mcmc_areas(poisson_model_draws, pars = "avg_birds_per_person") + 
  geom_vline(xintercept = 6, col = "orange", lwd = 2)

  

############################################
par(mfrow = c(1,1))
library(tidyverse)
library(palmerpenguins)

# inspect prior



true_mean = rnorm(1, mean = 18, sd = 3)
true_sd = rexp(1, rate = 1)
hist(rnorm(500, mean = true_mean, sd = true_sd))

# we can run it multiple times to inspect if its a prior that captures well what we think

penguins |> glimpse()

penguins |> 
  ggplot(aes(x=bill_depth_mm)) + 
  geom_histogram(binwidth = .5)


hist(rexp(5000, 1))



normal_dist <- cmdstan_model("normal_dist.stan")

penguins_noNA = penguins |>
  filter(!is.na(bill_depth_mm))




normal_dist_sample = normal_dist$sample(data = peng_data)

normal_dist_sample


library(ggdist)

normal_dist_draws <- normal_dist_sample$draws(variables = 'sigma')

normal_dist_draws |>
  posterior::as_draws_df(.) |>
  ggplot(aes(x = sigma))+
  stat_dotsinterval()



## Now we  build fake  data from the estimated mean and variance from the posterior distributions of the model
# and test how well they predict the real data


#######################################

# running model with 3 intercepts (anova), one for each species

penguins_noNA = penguins |>
  filter(!is.na(bill_depth_mm)) %>%
  mutate(spp_id = as.numeric(species)) 
  
peng_data <- list(N = length(penguins_noNA$bill_depth_mm),
                  measurements = penguins_noNA$bill_depth_mm,
                  spp_id = penguins_noNA$spp_id)


normal_dist_sp <- cmdstan_model("normal_dist_spp.stan")

normal_dist_sample = normal_dist_sp$sample(data = peng_data)

shinystan::launch_shinystan(normal_dist_sample)


spp_yrep_draws <- normal_dist_sample$draws(variables = c("yrep"))
spp_draws_matrix <- posterior::as_draws_matrix(spp_yrep_draws)

bayesplot::ppc_dens_overlay(y = peng_data$measurements,
                            yrep = head(spp_draws_matrix, 50))


