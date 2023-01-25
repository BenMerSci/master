library(rstan)
# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# The data
dataset <- readRDS("data/clean/new_dataset.RDS")

# Select desired variables
dataset <- dataset |>
             dplyr::select(biomass_flow, biomass_prey,
              abundance_predator, predator, pred_id)

# Fit the model
output_stan_model2 <- rstan::stan(
  file = "R/08_stan_model2.stan",
  iter = 4000,
  chains = 4,
  cores = 3,
  data = tidybayes::compose_data(dataset)
)

# Save it RDS
saveRDS(output_stan_model2, "results/model_outputs/output_stan_model2.RDS")

##### Analysis section #####
# Libraries
library(loo)
library(tidyverse)
library(tidybayes)
library(bayesplot)
library(ggdist)
library(ggridges)
library(gridExtra)
library(patchwork)
library(hrbrthemes)

# Load the model
dataset <- readRDS("data/clean/new_dataset.RDS")
source("R/10_plot_func.R")
output_stan_model2 <- readRDS("results/model_outputs/output_stan_model2.RDS")

# Recover types
output_stan_model2 <- recover_types(output_stan_model2)
# Draw the posterior for all parameters
general_params <- output_stan_model2 |>
                   tidybayes::gather_draws(mu_alpha, sd_alpha, sigma)

alpha <- output_stan_model2 |> tidybayes::gather_draws(alpha[pred_id])

# Get pred_id & habitat_type to join with the stan output
pred_ids <- unique(dataset[, c("pred_id", "habitat_type", "trophic_guild")])

alpha <-  dplyr::left_join(alpha, pred_ids, by = "pred_id") |>
          dplyr::mutate(pred_id = as.factor(pred_id),
                        habitat_type = as.factor(habitat_type),
                        trophic_guild = as.factor(trophic_guild))

pred_ids <- pred_ids |> dplyr::mutate(pred_id = as.factor(pred_id),
                                      habitat_type = as.factor(habitat_type),
                                      trophic_guild = as.factor(trophic_guild))

pred_ids_guild <- pred_ids |> dplyr::arrange(trophic_guild)

desired_order <- c("Small mammal herbivore","Large mammal herbivore","Small mammal predator",
                   "Medium mammal predator","Large mammal predator","Predatory birds",
                   "Non-predatory birds","Small reef-coast","Medium reef-coast",
                   "Small pelagic omnivore","Medium pelagic omnivore",
                   "Small pelagic carnivore","Medium pelagic carnivore",
                   "Small demersal omnivore","Medium demersal omnivore",
                   "Small demersal carnivore","Medium demersal carnivore",
                   "Reptiles","Sharks","Cephalopods","Shrimps",
                   "Mollusc and crustacea","Invertebrates","Plankton")

alpha$trophic_guild <- factor(as.character(alpha$trophic_guild), levels=desired_order)

alpha <- alpha[order(alpha$trophic_guild),]
alpha <- alpha |> dplyr::arrange(trophic_guild)

ggplot(alpha, aes(x = `.value`, y = pred_id, fill = trophic_guild)) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
     theme_ipsum() +
      theme(
       legend.title = element_text(size = 15),
       legend.text = element_text(size = 14),
       axis.title.x = element_text(size = 20),
       axis.text.x = element_text(size = 18),
       axis.text.y = element_blank()
      ) +
      xlim(c(-20, 10)) +
       ylab("") +
       xlab("Space clearance rate (km²/individual*year)") +
       labs(fill = "Trophic guild") +
        scale_y_discrete(guide = guide_axis(n.dodge = 2), limits = alpha$pred_id)# +
        #scale_color_manual(values=c("Small mammal herbivore"="palegreen",
        #                            "Large mammal herbivore"="palegreen3",
        #                            "Small mammal predator"="lightpink",
        #                            "Medium mammal predator"="lightcoral",
        #                            "Large mammal predator"="indianred4",
        #                            "Small reef-coast"="snow3",
        #                            "Medium reef-coast"="snow4",
        #                            "Non-predatory birds"="deepskyblue3",
        #                            "Predatory birds"="deepskyblue4",
        #                            "Small demersal carnivore"="",
        #                            "Medium demersal carnivore"="",
        #                            ""
        #                            ))

plot_sim_error(output_stan_model2)


# allometric relation
alpha_bodymass <- summary(output_stan_model2, pars = "alpha")$summary |>
                    as.data.frame() |>
                    dplyr::mutate(pred_id = c(1:154)) |>
                    dplyr::select(pred_id, mean) |>
                    dplyr::left_join(dataset, by="pred_id") |>
                    dplyr::select(pred_id, mean, bodymass_mean_predator) |>
                    unique()

alpha_bodymass |>
ggplot(aes(x = log(bodymass_mean_predator), y = mean)) +
        geom_point() +
        theme_minimal() +
        theme(
         legend.title = element_blank(),
         plot.title = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         axis.title.x = element_text(size = 15),
         axis.text.x = element_text(size = 13),
         axis.text.y = element_text(size = 13)
        ) +
        labs(title = "Relation of space clearance rate and bodymass") +
        xlab("Log bodymass value") +
        ylab("Alpha values") +
        geom_abline(intercept = 0, slope = 1)
