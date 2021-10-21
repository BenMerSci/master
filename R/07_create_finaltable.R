# Load the data needed

interactions <- readRDS("data/intermediate/resolved_inter_table.RDS")
traits <- readRDS("data/intermediate/species_traits.RDS")
enviro <- readRDS("data/intermediate/enviro_traits.RDS")

# Merge the tables together
dataset <- merge(interactions, traits, by.x = "prey", by.y = "scientific_name", all.x = TRUE) |>
	dplyr::rename(metabolism_prey = "metabolic_class", bodymass_prey = "bodymass", bodymass_min_prey = "bodymass_min", speed_prey = "speed") |>
	dplyr::select(-"gbif_id") |>
	merge(traits, by.x = "predator", by.y = "scientific_name", all.x = TRUE) |>
	dplyr::rename(metabolism_pred = "metabolic_class", bodymass_pred = "bodymass", bodymass_min_pred = "bodymass_min", speed_pred = "speed") |>
	dplyr::select(-"gbif_id") |>
	merge(enviro, by.x = "model_name", by.y = "model_name", all.x = TRUE) |>
	dplyr::select("prey","predator","energy_flow","flux_units","biomass_prey",
	"biomass_pred","bodymass_prey","bodymass_min_prey","bodymass_pred","bodymass_min_pred","metabolism_prey","metabolism_pred",
	"speed_prey", "speed_pred","model_name","model_year","ecosystem_type","habitat_type",
	"water_temperature","air_temperature")
	
saveRDS(data, "data/clean/dataset.RDS")
