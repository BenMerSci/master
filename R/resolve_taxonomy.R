# Load the interactions table
inter_table <- readRDS("data/inter_table.RDS")

# Get a unique vector of the species names
prelem_sp_list <- data.frame(unique(c(inter_table$species_from, inter_table$species_to))) |>
		dplyr::rename(original_name = "unique.c.inter_table.species_from..inter_table.species_to..")

# Resolve the names
# If file of resolved_names doesn't exist it creates it, otherwise it loads it
if(!file.exists("data/resolved_names.RDS")) {
	resolved_names <- taxize::gnr_resolve(prelem_sp_list$original_name, canonical = TRUE, best_match_only = TRUE, cap_first = TRUE)
	saveRDS(resolved_names, "data/resolved_names.RDS")
} else{
	resolved_names <-  readRDS("data/resolved_names.RDS")
}

# Get the unknown names and bind them back into the resolved data_frame
unknown_names <- data.frame(attributes(resolved_names)$not_known) |>
		 dplyr::rename(user_supplied_name = "attributes.resolved_names..not_known")

resolved_names <- dplyr::bind_rows(resolved_names, unknown_names)

# Replace names that had bad matches
resolved_names[which(resolved_names[,"user_supplied_name"] == "Small Zooplankton"),"matched_name2"] <- "Small Zooplankton"
resolved_names[which(resolved_names[,"user_supplied_name"] == "Physeter macrocephalus"),"matched_name2"] <- "Physeter macrocephalus"

# Replace the ones that had no matches
resolved_names[which(is.na(resolved_names$matched_name2)),"matched_name2"] <- resolved_names[which(is.na(resolved_names$matched_name2)),"user_supplied_name"]

# Select only desired columns
resolved_names <- dplyr::select(resolved_names, c("user_supplied_name", "matched_name2"))

# Merge back the corrected names into inter_table
resolved_inter_table <- base::merge(inter_table, resolved_names, by.x = "species_from", by.y = "user_supplied_name", all.x = TRUE) |>
	dplyr::select(c("model_name","matched_name2","species_to","energy_flow")) |>
	dplyr::rename(species_from = "matched_name2") |>
	base::merge(resolved_names, by.x = "species_to", by.y = "user_supplied_name", all.x = TRUE) |>
	dplyr::select("model_name","species_from","matched_name2","energy_flow") |>
	dplyr::rename(species_to = "matched_name2")

# Save it back into inter_table
saveRDS(resolved_inter_table, file = "data/resolved_inter_table.RDS")
