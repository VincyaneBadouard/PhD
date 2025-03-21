install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))

library(cmdstanr)
library(posterior)
library(bayesplot)

check_cmdstan_toolchain() # The C++ toolchain required for CmdStan is setup properly!

install_cmdstan(cores = 2)

file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan") # path for the stan file
mod <- cmdstan_model(file) # the model

mod$print() # Stan program
mod$exe_file() # path to the compiled executable

# names correspond to the data block in the Stan program
data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))

fit <- mod$sample( # Running MCMC with 4 parallel chains
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

# Posterior summary statistics
# $summary() method calls summarise_draws() from the posterior package
fit$summary()
fit$summary(variables = c("theta", "lp__"), "mean", "sd")

# use a formula to summarize arbitrary functions, e.g. Pr(theta <= 0.5)
fit$summary("theta", pr_lt_half = ~ mean(. <= 0.5))

# summarise all variables with default and additional summary measures
fit$summary(
  variables = NULL,
  posterior::default_summary_measures(),
  extra_quantiles = ~posterior::quantile2(., probs = c(.0275, .975))
)

# Posterior draws
# https://mc-stan.org/cmdstanr/articles/posterior.html
draws_arr <- fit$draws() # or format="array"
str(draws_arr)

# draws x variables data frame
draws_df <- fit$draws(format = "df")
str(draws_df)
print(draws_df)

# Plotting posterior draws
mcmc_hist(fit$draws("theta"))

fit$sampler_diagnostics() # Sampler diagnostics
fit$diagnostic_summary() # diagnose warnings

# Saving fitted model objects
fit$save_object(file = "fit.RDS")
# can be read back in using readRDS
fit2 <- readRDS("fit.RDS")

