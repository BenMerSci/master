# Load the data needed

interactions <- readRDS("data/intermediate/resolved_inter_table.RDS")
traits <- readRDS("data/intermediate/species_traits.RDS")
enviro <- readRDS("data/intermediate/enviro_traits.RDS")


# Merge the tables together
dataset <- merge(interactions, traits, by.x = "prey", by.y = "scientific_name", all.x = TRUE) |>
	dplyr::rename(organism_class_prey = "organism_class", bodymass_prey = "bodymass", speed_prey = "speed") |>
	dplyr::select(-"gbif_id") |>
	merge(traits, by.x = "predator", by.y = "scientific_name", all.x = TRUE) |>
	dplyr::rename(organism_class_pred = "organism_class", bodymass_pred = "bodymass", speed_pred = "speed") |>
	dplyr::select(-"gbif_id") |>
	merge(enviro, by.x = "model_name", by.y = "model_name", all.x = TRUE) |>
	dplyr::select("prey","predator","energy_flow","flux_units","biomass_prey","bodymass_prey",
	"organism_class_prey","speed_prey","biomass_pred","bodymass_pred","organism_class_pred", "speed_pred",
	"model_name","model_year","ecosystem_type","habitat_type",
	"water_temperature","air_temperature")

# Compute predator-prey mass ratio
dataset$bodymass_prey <- as.numeric(dataset$bodymass_prey)
dataset$bodymass_pred <- as.numeric(dataset$bodymass_pred)
dataset$ppmr <- dataset$bodymass_pred/dataset$bodymass_prey

# Only get the interactions in the same fluxes units
# t/km2 is the same as g/m2
dataset <- dataset[which(dataset$flux_units %in% c("t/km2/yr","g/m2/yr","g/m2/yr, dry","kg/km2/yr","kg/km2/yr, dry")),]

# Save the dataset
saveRDS(dataset, "data/clean/dataset.RDS")

