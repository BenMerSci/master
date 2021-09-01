# Load the interaction table to make a unique species list
resolved_inter_table <- readRDS("data/resolved_inter_table.RDS")

species_list <- data.frame(unique(c(resolved_inter_table$species_from, resolved_inter_table$species_to))) |>
		dplyr::rename(scientific_name = "unique.c.resolved_inter_table.species_from..resolved_inter_table.species_to..")

# Get the GBIF IDs for each species
species_list$gbif_id <- apply(species_list, 1, function(x) taxize::get_gbifid(x, ask = TRUE, messages = TRUE)[1])

# Get hierarchy for each species from their gbif_id
metabolic_class <- lapply(species_list[,"gbif_id"], function(x) taxize::classification(x, db = "gbif")) |>
		lapply(function(x) data.frame(x[[1]])) |>
		lapply(function(x) x[which(x$rank == "class"),"name"]) |>
		lapply(function(x) ifelse(is.null(x), NA, x))

metabolic_class <- do.call("rbind", metabolic_class)

# Bind the classes to the species list
species_list <- cbind(species_list, metabolic_class)

# Uniformize it
species_list[is.na(species_list$metabolic_class),"metabolic_class"] <- "Plankton"


#test <- ifelse(species_list$metabolic_class %in% c("Actinopterygii", ))