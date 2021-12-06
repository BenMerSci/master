# Load the data needed

interactions <- readRDS("data/intermediate/resolved_inter_table.RDS")
traits <- readRDS("data/intermediate/species_traits.RDS")
enviro <- readRDS("data/intermediate/enviro_traits.RDS")


# Merge the tables together
dataset <- merge(interactions, traits, by.x = "prey", by.y = "scientific_name", all.x = TRUE) |>
	dplyr::rename(class_prey = "class", family_prey = "family", bodymass_prey = "bodymass", speed_prey = "speed") |>
	dplyr::select(-"gbif_id") |>
	merge(traits, by.x = "predator", by.y = "scientific_name", all.x = TRUE) |>
	dplyr::rename(class_pred = "class", family_pred = "family", bodymass_pred = "bodymass", speed_pred = "speed") |>
	dplyr::select(-"gbif_id") |>
	merge(enviro, by.x = "model_name", by.y = "model_name", all.x = TRUE) |>
	dplyr::select("prey","predator","energy_flow","flux_units","biomass_prey","bodymass_prey",
	"class_prey", "family_prey","speed_prey","biomass_pred","bodymass_pred","class_pred","family_pred","speed_pred",
	"model_name","model_year","ecosystem_type","habitat_type",
	"water_temperature","air_temperature")

# Compute predator-prey mass ratio
dataset$bodymass_prey <- as.numeric(dataset$bodymass_prey)
dataset$bodymass_pred <- as.numeric(dataset$bodymass_pred)
dataset$ppmr <- dataset$bodymass_pred/dataset$bodymass_prey

# Only get the interactions in the same fluxes units
# t/km2 is the same as g/m2
dataset <- dataset[which(dataset$flux_units %in% c("t/km2/yr","g/m2/yr","g/m2/yr, dry","kg/km2/yr","kg/km2/yr, dry")),]
# Convert the ones in kg/km2/yr to t/km2/yr
dataset[which(dataset$flux_units %in% c("kg/km2/yr","kg/km2/yr, dry")),"energy_flow"] <- (dataset[which(dataset$flux_units %in% c("kg/km2/yr","kg/km2/yr, dry")), "energy_flow"])/1000

# Remove interaction that are between plantkons
# So we focus on interactions wich involves at least
# one actual species
dataset <- dataset[-which(grepl("plankton", dataset$prey) & grepl("plankton", dataset$predator)),]

# Dimensionality
#  Fish eating fish classified as 3d because of the environment
dataset[which(dataset$class_prey %in% c("Actinopterygii","Elasmobranchii", "Sarcopterygii") & dataset$class_pred %in% c("Actinopterygii","Elasmobranchii", "Sarcopterygii")),"dimensionality"] <- "3d"
# Birds eating small mammals should see them as 2d from up in the air
dataset[which(dataset$class_pred == "Aves" &  dataset$class_prey == "Mammalia" & dataset$habitat_type == "terrestrial"), "dimensionality"] <- "2d"
# Whales eating plankton in 2d, as whales are basically going through water layer to layer catching it
dataset[which(dataset$family_pred %in% c("Balaenopteridae","Physeteridae") & dataset$class_prey == "Plankton" & dataset$habitat_type == "marine"),"dimensionality"] <- "2d"
# Terrestrial mammals predator
dataset[which(dataset$class_pred == "Mammalia" & dataset$habitat_type == "terrestrial"), "dimensionality"] <- "3d"
# Terrestrial birsd predator
dataset[which(dataset$class_pred == "Aves" & dataset$habitat_type == "terrestrial"), "dimensionality"]<- "2d"
# If fish, interaction in 3d
dataset[which(dataset$habitat_type %in% c("freshwater","marine") & dataset$class_pred %in% c("Actinopterygii","Elasmobranchii", "Sarcopterygii")), "dimensionality"] <- "3d"
# Marine mammals that are not whales, in 3d
dataset[which(dataset$class_pred == "Mammalia" & dataset$family_pred != "Balaenopteridae" & dataset$habitat_type == "marine"),"dimensionality"] <- "3d"
# Aves eating fish
dataset[which(dataset$class_pred == "Aves" & dataset$habitat_type == "marine"),"dimensionality"] <- "2d"
# Remaining interaction in 3d
dataset[which(is.na(dataset$dimensionality)),"dimensionality"] <- "3d"

# Save the dataset
saveRDS(dataset, "data/clean/dataset.RDS")

