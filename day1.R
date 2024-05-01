


hist(rpois(1000, 0.4))

curve(dpois(10000, 2))

dnorm(0, 1)


# 1

hist(rpois(1000, 0.4))

# 2

hist(rpois(1000, 0.7))

curve(dpois(x, 7), xlim = (0,15), n = 30, type = "b")


rocks = read.csv("rocks.csv")

View(rocks)


hist(rocks$jumps)

# General


## both hands

hist(rocks$jumps)



par(mfrow = c(3,1))

# hypothesis

hist(rpois(1000, 0.7), main = "hypothesis", xlim = c(0,6))

## good hand


hist(rocks[which(rocks$hand == "good"),"jumps"],  main = "good hand", xlim = c(0,6))

## bad hand

hist(rocks[which(rocks$hand == "bad"),"jumps"], main = "bad hand", xlim = c(0,6))


max(rocks$jumps)

# By person

par(mfrow = c(4,2))

## Alex

hist(rocks[which(rocks$hand == "good" & rocks$person == "alex"),"jumps"], main = "good hand", col = "red", xlim = c(0,6))

hist(rocks[which(rocks$hand == "bad" & rocks$person == "alex"),"jumps"], main = "bad hand", col = "red", xlim = c(0,6))

## Rosalie

hist(rocks[which(rocks$hand == "good" & rocks$person == "rosalie"),"jumps"], main = "good hand", col = "green", xlim = c(0,6))

hist(rocks[which(rocks$hand == "bad" & rocks$person == "rosalie"),"jumps"], main = "bad hand", col = "green", xlim = c(0,6))

## Joseppe

hist(rocks[which(rocks$hand == "good" & rocks$person == "joseppe"),"jumps"], main = "good hand", col = "pink", xlim = c(0,6))

hist(rocks[which(rocks$hand == "bad" & rocks$person == "joseppe"),"jumps"], main = "bad hand", col = "pink", xlim = c(0,6))

#aymeric

hist(rocks[which(rocks$hand == "good" & rocks$person == "aymeric"),"jumps"], main = "good hand", col = "blue", xlim = c(0,6))

hist(rocks[which(rocks$hand == "bad" & rocks$person == "aymeric"),"jumps"], main = "bad hand", col = "blue", xlim = c(0,6))


#moments of the distribution

n = length(rocks[which(rocks$hand == "good" & rocks$person == "joseppe"),"jumps"])
mean = mean(rocks[which(rocks$hand == "good" & rocks$person == "joseppe"),"jumps"])




hist(rpois(n, mean), xlim = c(0,6))


a = dpois(0,  mean(rocks[which(rocks$hand == "good" & rocks$person == "alex"),"jumps"]))
b = dpois(0,  mean(rocks[which(rocks$hand == "good" & rocks$person == "joseppe"),"jumps"]))
c = dpois(0,  mean(rocks[which(rocks$hand == "good" & rocks$person == "rosalie"),"jumps"]))
d = dpois(0,  mean(rocks[which(rocks$hand == "good" & rocks$person == "aymeric"),"jumps"]))



install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(cmdstanr)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")


check_cmdstan_toolchain()
check_cmdstan_toolchain(fix = TRUE)
install_cmdstan(cores = 2)
cmdstan_path()
cmdstan_version()
file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstan_model(file)
mod$print()

# names correspond to the data block in the Stan program
data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))

fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)
