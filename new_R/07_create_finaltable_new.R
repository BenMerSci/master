# Load the data needed
interactions <- readRDS("data/intermediate/new/resolved_inter_table.RDS")
traits <- readRDS("data/intermediate/new/species_traits_new.RDS")
enviro <- readRDS("data/intermediate/new/enviro_traits.RDS")

# Merge the tables together
dataset <- merge(interactions, traits, by.x = "prey",
            by.y = "scientific_name", all.x = TRUE) |>
           dplyr::rename(class_prey = "class", family_prey = "family",
            metabolism_prey = metabolic_class, bodymass_min_prey = bodymass_min,
            bodymass_mean_prey = bodymass_mean, bodymass_max_prey = bodymass_max) |>
           dplyr::select(-"gbif_id") |>
           merge(traits, by.x = "predator", by.y = "scientific_name",
            all.x = TRUE) |>
           dplyr::rename(class_predator = "class", family_predator = "family",
            metabolism_predator = metabolic_class, bodymass_min_predator = bodymass_min,
            bodymass_mean_predator = bodymass_mean, bodymass_max_predator = bodymass_max) |>
           dplyr::select(-"gbif_id") |>
           merge(enviro, by.x = "model_name", by.y = "model_name",
            all.x = TRUE) |>
           dplyr::rename(c(flux_units = "currency_units")) |>
           dplyr::select("prey", "predator", "pred_flow",
            "flux_units", "biomass_prey", "bodymass_min_prey",
            "bodymass_mean_prey", "bodymass_max_prey", "class_prey",
            "family_prey", "metabolism_prey", "degree_predator",
            "biomass_predator", "bodymass_min_predator", "bodymass_mean_predator",
            "bodymass_max_predator", "class_predator", "family_predator",
            "metabolism_predator", "model_name", "model_year", "ecosystem_type",
            "habitat_type", "water_temperature", "air_temperature")

# Change species mass from grams to tons
dataset$bodymass_min_prey <- dataset$bodymass_min_prey * 0.000001
dataset$bodymass_mean_prey <- dataset$bodymass_mean_prey * 0.000001
dataset$bodymass_max_prey <- dataset$bodymass_max_prey * 0.000001
dataset$bodymass_min_predator <- dataset$bodymass_min_predator * 0.000001
dataset$bodymass_mean_predator <- dataset$bodymass_mean_predator * 0.000001
dataset$bodymass_max_predator <- dataset$bodymass_max_predator * 0.000001

# Only keep interactions in which the prey are also predators,
# to only keep all the interactions of the predators
#test <- dataset[!(dataset$predator %in% dataset$prey), ]
#
#index <- unique(dataset[(dataset$prey %in% dataset$predator), "prey"])
#test1 <- dataset[which(dataset$predator %in% index), ]

# Save the dataset
saveRDS(dataset, "data/clean/new_dataset.RDS")
