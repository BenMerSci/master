# Load the interactions table
inter_table <- readRDS("data/intermediate/inter_table.RDS")

# Get a unique vector of the species names
prelem_sp_list <- tibble::enframe(x = unique(c(inter_table$prey, inter_table$predator)),
 name = NULL, value = "original_name")

# Resolve the names
resolved_names <- taxize::gnr_resolve(prelem_sp_list$original_name, best_match_only = TRUE) |>
  tibble::as_tibble()

# Join the original list of species with the resolved ones
species <- dplyr::left_join(prelem_sp_list, resolved_names, by = c("original_name" = "user_supplied_name")) |>
    dplyr::select("original_name","matched_name") |>
    dplyr::rename(scientific_name = "matched_name")

# Replace names that did not correctly match with valid name
species[which(species$original_name == "Crustacea and Molluscs"), "scientific_name"] <- "Crustacea and Molluscs"
species[which(species$original_name == "Bagrus dogmac"), "scientific_name"] <- "Bagrus docmak"
species[which(species$original_name == "Luciolates stappersii"), "scientific_name"] <- "Lates stappersii"
species[which(species$original_name == "Stolothrissa tanganyicae"), "scientific_name"] <- "Stolothrissa tanganicae"
species[which(species$original_name == "Hydrocynus forskalii"), "scientific_name"] <- "Hydrocynus forskahlii"
species[which(species$original_name == "Small pelagic fish"), "scientific_name"] <- "Small pelagic fish"
species[which(species$original_name == "Anser caerulescens atlantica"), "scientific_name"] <- "Anser caerulescens atlanticus"
species[which(species$original_name == "Sciades guatemalensis"), "scientific_name"] <- "Ariopsis guatemalensis"
species[which(species$original_name == "Small catfish"), "scientific_name"] <- "Small catfish"
species[which(species$original_name == "Medium cephalopod"), "scientific_name"] <- "Medium cephalopod"
species[which(species$original_name == "Small cephalopod"), "scientific_name"] <- "Small cephalopod"
species[which(species$original_name == "Small demersal fish"), "scientific_name"] <- "Small demersal fish"
species[which(species$original_name == "Synodus sp"), "scientific_name"] <- "Synodus sp"
species[which(species$original_name == "Rays Sharks"), "scientific_name"] <- "Rays Sharks"
species[which(species$original_name == "Cynoponctius coniceps"), "scientific_name"] <- "Cynoponctius coniceps"
species[which(species$original_name == "Auxis thazard thazard"), "scientific_name"] <- "Auxis thazard"
species[which(species$original_name == "Arius bilineatus"), "scientific_name"] <- "Netuma bilineata"
species[which(species$original_name == "Euryglossa orientalis"), "scientific_name"] <- "Brachirus orientalis"
species[which(species$original_name == "Stolephorus heterolobus"), "scientific_name"] <- "Encrasicholina heteroloba"
species[which(species$original_name == "Opsaridium microcephalus"), "scientific_name"] <- "Opsaridium microcephalum"
species[which(species$original_name == "Bubo scandiaca"), "scientific_name"] <- "Bubo scandiacus"
species[which(species$original_name == "Ursus horribilis"), "scientific_name"] <- "Ursus arctos"

# Replace names that had no match with original_name
species[which(is.na(species$scientific_name)), "scientific_name"] <- species[which(is.na(species$scientific_name)),"original_name"]

# Merge into the table
resolved_inter_table <- base::merge(inter_table, species, by.x = "prey",
                          by.y = "original_name", all.x = TRUE) |>
                        dplyr::select(c("model_name", "scientific_name", "predator",
                          "biomass_flow", "biomass_prey",
                          "biomass_predator", "degree_predator")) |>
                        dplyr::rename(prey = "scientific_name") |>
                        base::merge(species, by.x = "predator",
                          by.y = "original_name", all.x = TRUE) |>
                        dplyr::select("model_name", "prey", "scientific_name",
                          "biomass_flow", "biomass_prey",
                          "biomass_predator", "degree_predator") |>
                        dplyr::rename(predator = "scientific_name")

# Save it back into inter_table
saveRDS(resolved_inter_table,
 file = "data/intermediate/resolved_inter_table.RDS"
)
