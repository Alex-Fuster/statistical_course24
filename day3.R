
par(mfrow = c(1,1))

hist(rgamma(n=500, shape = 2, rate = 1, scale = 1))

hist(rbeta(500, 1, 2))


a = rnorm(1, mean = -1, sd = 5)
hist(rpois(500, exp(a)))


tibble(
  
  means = seq(from = -3, to = 30, length.out = 12)
  
)|>
  rowise() |>
  mutate(obs = list(rpois(200, lambda = exp(means)))) 


curve(1/(1+exp(-x)), xlim = c(-3,3), ylim = c(0,1))


a <- rnorm(1, mean = 0, 1)

hist(rbinom(500, 20, prob = 1/(1+exp(a))))
  


################
library(palmerpenguins)
library(tidyverse)
library(cmdstanr)
library(ggplot2)
library(tidybayes)

penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point() + 
  stat_smooth(method = "lm")

bill_line <- coef(lm(bill_depth_mm ~ bill_length_mm, data = penguins))



bill_len_centered <- with(penguins,
                          bill_length_mm - mean(bill_length_mm,
                                                na.rm = TRUE))

## make up a short vector
some_bill_lengths <- seq(
  from = min(bill_len_centered, na.rm = TRUE), 
  to = max(bill_len_centered, na.rm = TRUE),
  length.out = 10
)

slopes <- rnorm(7, 0, .5)
inters <- rnorm(7, 17, 2)

X <- cbind(1, some_bill_lengths)
B <- rbind(inters, slopes)

knitr::kable(head(X))

knitr::kable(head(B))

prior_mus <- X %*% B

matplot(x = some_bill_lengths,
        y = prior_mus, type = "l")



### USE THE FULL MODEL TO SIMULATE OBSERVATIONS

slopes <- rnorm(7, 0, .5)
inters <- rnorm(7, 17, 2)
sigmas <- rexp(7, rate = 0.3)

X <- cbind(1, some_bill_lengths)
B <- rbind(inters, slopes)

prior_mus <- X %*% B

prior_obs <- matrix(0, nrow = nrow(prior_mus), ncol = ncol(prior_mus))

for (j in 1:ncol(prior_obs)) {
  prior_obs[,j] <- rnorm(n = nrow(prior_mus),
                         mean = prior_mus[,j],
                         sd = sigmas[j])
}

matplot(x = some_bill_lengths,
        y = prior_obs, type = "p")


################

tibble(
  sim_id = 1:7,
  slopes = rnorm(7, 0, .5),
  inters = rnorm(7, 17, 2),
  sigmas = rexp(7, rate = 2)
) |> 
  mutate(x = list(seq(from = -10, to = 10, length.out = 6))) |> 
  rowwise() |> 
  mutate(avg = list(x * slopes + inters),
         obs = list(rnorm(length(avg), mean = avg, sd = sigmas)),
         sim_id = as.factor(sim_id)) |> 
  unnest(cols = c("x", "avg", "obs")) |> 
  ggplot(aes(x= x, y = avg, group = sim_id, fill = sim_id)) + 
  geom_line(aes(colour = sim_id)) + 
  geom_point(aes(y = obs, fill = sim_id), pch = 21, size = 3) + 
  scale_fill_brewer(type = "qual") + 
  scale_colour_brewer(type = "qual") + 
  facet_wrap(~sim_id)


#########

# prior predictions
# posterior predictions

# adding regression line


normal_regression <- cmdstan_model(stan_file = "normal_regression_nullpred.stan")


## drop NAs
penguins_no_NA <- penguins |> 
  tidyr::drop_na(bill_depth_mm, bill_length_mm) |> 
  dplyr::mutate(
    bill_length_center = bill_length_mm - mean(bill_length_mm))

## assemble data list
data_list <- with(penguins_no_NA,
                  list(N = length(bill_length_center),
                       bill_len = bill_length_center,
                       bill_dep = bill_depth_mm
                  ))


normal_reg_no_pred <- normal_regression$sample(
  data = data_list, 
  refresh = 0)


normal_reg_no_pred$draws() |> 
  bayesplot::mcmc_areas(pars = c("slope", "intercept", "sigma"))


#############


normal_regression <- cmdstan_model(stan_file = "normal_regression_nullpred.stan")

penguins_no_NA <- penguins |> 
  tidyr::drop_na(bill_depth_mm, bill_length_mm) |> 
  dplyr::mutate(
    bill_length_center = bill_length_mm - mean(bill_length_mm))

data_list <- with(penguins_no_NA,
                  list(N = length(bill_length_center),
                       bill_len = bill_length_center,
                       bill_dep = bill_depth_mm,
                       npost = 6,
                       pred_values = modelr::seq_range(penguins_no_NA$bill_length_center, n = 6)
                  ))

bill_norm_reg <- normal_regression$sample(data = data_list, 
                                          refresh = 0)



## Plot


bill_posterior <- bill_norm_reg |> 
  tidybayes::spread_rvars(post_bill_dep_average[i],
                          post_bill_dep_obs[i]) |>
  mutate(bill_length = data_list$pred_values[i]) 

bill_posterior |> 
  ggplot(aes(x = bill_length, dist = post_bill_dep_average)) + 
  tidybayes::stat_lineribbon() + 
  geom_point(aes(x = bill_length_center, y = bill_depth_mm),
             data = penguins_no_NA, 
             inherit.aes = FALSE) + 
  scale_fill_brewer(palette = "Greens", direction = -1, guide = "none") + 
  labs(title = "Average response")
bill_posterior |> 
  ggplot(aes(x = bill_length, dist = post_bill_dep_obs)) + 
  tidybayes::stat_lineribbon() + 
  geom_point(aes(x = bill_length_center, y = bill_depth_mm),
             data = penguins_no_NA, 
             inherit.aes = FALSE) + 
  scale_fill_brewer(palette = "Greens", direction = -1, guide = "none") +
  labs(title = "Predicted observations")


################################


# for different species

normal_regression <- cmdstan_model(stan_file = "normal_regression_nullpred_sp.stan")
