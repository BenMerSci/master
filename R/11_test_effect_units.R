library(lme4)
library(merTools)
library(ggplot2)
library(dplyr)
dataset <- readRDS("data/clean/new_dataset.RDS")

# Keep desired variables
dataset <- dataset |> dplyr::select(pred_flow, biomass_prey, abundance_predator, flux_units)
# Correct the units for the dry weight communities -> Dry Weight and NA (terrestrial which were Dry weight)
dataset[which(is.na(dataset$flux_units)), "flux_units"] <- "Dry weight (g/m^2)"

# Check how pred_flow varies by type of units
boxplot(log(pred_flow) ~ flux_units, data=dataset)

test_lm <- lmer(log(pred_flow) ~ log(biomass_prey) + log(abundance_predator) + (1|flux_units), data = dataset)

summary(test_lm)

pred <- cbind(dataset, predictInterval(test_lm, dataset))

ggplot(pred) +
  geom_line(aes(log(biomass_prey)+log(abundance_predator), fit, fill = flux_units)) +
  geom_ribbon(aes(log(biomass_prey)+log(abundance_predator), ymin = lwr, ymax = upr, fill = flux_units), alpha = .2) +
  geom_point(aes(log(biomass_prey)+log(abundance_predator) , y = log(pred_flow), color = flux_units))
