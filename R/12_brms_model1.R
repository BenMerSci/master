# Needed libraries
library(tidyverse)
library(tidybayes)
library(brms)
library(rstan)

# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load the dataset
dataset <- readRDS("data/clean/dataset.RDS")
dataset_sub <- subset(dataset, select = c("pred_flow","abund_prey","abund_pred","bodymass_prey"))


# Gamma model
curve_bf_gamma <- bf(pred_flow ~ (a * abund_prey * bodymass_prey * abund_pred), 
                    family = Gamma(link = "identity"),
                    a ~ 1,
                    nl = TRUE)

get_prior(curve_bf_gamma, data = dataset_sub)

curve_gamma_prior <- c(
    prior(gamma(6.25, 0.25), class = "shape"),
    prior(normal(1,1), class = "b", nlpar = "a")
)

curve_gamma_post <- brm(curve_bf_gamma, 
                        prior = curve_gamma_prior, 
                        data = dataset_sub)

plot(curve_gamma_post)  

gamma_post <- dataset_sub |> 
  add_predicted_draws(curve_gamma_post)

gamma_post |> 
  ggplot(aes(x = abund_prey*abund_pred*bodymass_prey, y = .prediction)) + 
  stat_lineribbon() + 
  coord_trans(x = "log", y = "log") + 
  scale_fill_brewer(palette = "Oranges") + 
  geom_point(aes(x = abund_prey*abund_pred*bodymass_prey, y = pred_flow),
             size = 3, pch = 21, fill = "lightblue", data = dataset_sub)

# Lognormal model
curve_bf_lgnorm <- bf(log(pred_flow) ~ loga + log(abund_prey) + log(bodymass_prey) + log(abund_pred),
                     family = gaussian(link = "identity"),
                     loga ~ 1,
                     nl = TRUE)

get_prior(curve_bf_lgnorm, data = dataset_sub)

curve_lgnorm_prior <- c(
    prior(normal(0,5), class = "sigma"),
    prior(normal(1,1), class = "b", nlpar = "loga")
)

curve_lgnorm_post <- brm(curve_bf_lgnorm,
                               prior = curve_lgnorm_prior,
                               data = dataset_sub)

plot(curve_lgnorm_post)
#launch_shinystan(flux_curve_model_posterior)

lgnorm_post <- dataset_sub |> 
  add_predicted_draws(curve_lgnorm_post)


lgnorm_post |> 
  ggplot(aes(x = log(abund_prey)+log(abund_pred)+log(bodymass_prey), y = .prediction)) + 
  stat_lineribbon() + 
  coord_cartesian() + 
  scale_fill_brewer(palette = "Oranges") + 
  geom_point(aes(x = log(abund_prey)+log(abund_pred)+log(bodymass_prey), y = log(pred_flow)),
             size = 3, pch = 21, fill = "lightblue", data = dataset_sub)
  

# summary
summary(curve_lgnorm_post)

