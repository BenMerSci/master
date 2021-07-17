# Script to format the matrices into a list of pairwise interactions

# Load the matrices
load("data_retrieval/ecopath/Data_ncomms12573/DIET.Rdata")
load("data_retrieval/ecopath/Data_ncomms12573/Ecopath_models.Rdata")

# Wide to long format
DIET <- lapply(DIET, function(x) {
		base::as.data.frame(x) |> 
		tibble::rownames_to_column("X") |>
		tidyr::pivot_longer(!X, names_to = "species_to", values_to = "energy_flow") |>
		dplyr::rename(species_from = "X")
})

# Conserve ntw 33, 57, 64, 89, 106, 
temp_list <- DIET[c(33,57,64,89,106,110,111,112,113,114,115,116)]

# Remove the letters in some of the number IDs
DIET <- lapply(rapply(DIET, function(x) base::gsub("F", "", x), how = "list"), base::as.data.frame)

# Changing the id number in DIET for species_to, since their number started at 2 by defaut
# Have to substract 1 to each of them to get them back to starting from 1
DIET <- lapply(DIET, function(x) {
	x$species_to <- (as.numeric(x$species_to)-1)
	return(x)
	})

# Replace the previously save networks with their altered version
DIET[c(33,57,64,89,106,110,111,112,113,114,115,116)] <- temp_list

# Remove the letter X in some of the number IDs
# Done that after substracting 1 to the other networks, because the networks with X in the IDs were indexed starting from 1 and not from 2 like the previous one
DIET <- lapply(rapply(DIET, function(x) base::gsub("X", "", x), how = "list"), base::as.data.frame)
DIET <- lapply(rapply(DIET, function(x) base::gsub("V", "", x), how = "list"), base::as.data.frame)


# Loading the names to join them into the flow matrices
GroupName <- readRDS("data/list_names.RDS") |>
	lapply(function (x) { 
	     base::as.data.frame(x) |>
	     tibble::rownames_to_column("ID") |>
	     #dplyr::select(c("ID","scientific_name")) |>
	     na.omit()
	})

GroupName_terrestrial <- GroupName[c(110,111,112,113,114,115,116)]

GroupName <- lapply(GroupName, function(x) dplyr::select(x, c("ID", "scientific_name")))

# Save the arctic terrestrial networks
terrestrial_ntw <- DIET[c(110,111,112,113,114,115,116)]

# Join the names by ID into the flow matrices for "species_from"
# Both next methods work, either with baseR or Tidyverse
#DIET <- purrr::map2(DIET, GroupName, ~ dplyr::right_join(.x, .y, by = c("species_from" = "ID")))
DIET <- purrr::map2(DIET, GroupName, ~ merge(.x, .y, by.x = "species_from", by.y = "ID", all.y = TRUE, sort = FALSE))

# Reorder and rename the dataframes
DIET <- lapply(DIET, function(x) {
	as.data.frame(x) |>
	dplyr::select(c("scientific_name","species_to","energy_flow")) |>
	dplyr::rename(species_from = "scientific_name")
})

# Join again the names by ID into the flow matrices for "species_to"
DIET <- purrr::map2(DIET, GroupName, ~ merge(.x, .y, by.x = "species_to", by.y = "ID", all = TRUE, sort = FALSE))

# Reorder and rename the dataframes
DIET <- lapply(DIET, function(x) {
	dplyr::select(x, c("species_from","scientific_name","energy_flow")) |>
	dplyr::rename(species_to = "scientific_name") |>
	na.omit()
})

# Keep only the flow that are > 0
DIET <- lapply(DIET, function(x) {
	x <- x[which(x$energy_flow > 0),]
	return(x)
})

# Change names in the terrestrial network ntw 3 Voles
GroupName_terrestrial[[5]] <- rbind(GroupName_terrestrial[[5]], data.frame(ID = c("11","19","24"), original_name = c("Tundra_voles","Wolverine","Peregrine_falcon"), scientific_name = c("Microtus oeconomus","Gulo gulo","Falco peregrinus")))
GroupName_terrestrial[[6]] <- rbind(GroupName_terrestrial[[6]], data.frame(ID = c("7","11","12","13"), original_name = c("Brown_lemmings","Glaucus_gulls","Stoats", "Arctic_Foxes"), scientific_name = c("Lemnus trimucronatus","Larus hyperboreus","Mustela erminea","Vulpes lagopus")))

GroupName_terrestrial[1] <- lapply(rapply(GroupName_terrestrial[1], function(x) base::gsub("Long-tailed_jaeger", "Long-tailed Jaegers", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[1] <- lapply(rapply(GroupName_terrestrial[1], function(x) base::gsub("Arctic_wolve", "Arctic wolves", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[1] <- lapply(rapply(GroupName_terrestrial[1], function(x) base::gsub("Arctic_hare", "Arctic hare", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[1] <- lapply(rapply(GroupName_terrestrial[1], function(x) base::gsub("Collared_lemming", "Collared lemmings", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[2] <- lapply(rapply(GroupName_terrestrial[2], function(x) base::gsub("Arctic_fox", "Arctic Fox", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[3] <- lapply(rapply(GroupName_terrestrial[3], function(x) base::gsub("Rough-legged_hawk", "Rough legged hawk", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[3] <- lapply(rapply(GroupName_terrestrial[3], function(x) base::gsub("Red_fox", "Red fox", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[3] <- lapply(rapply(GroupName_terrestrial[3], function(x) base::gsub("Voles", "oles", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[3] <- lapply(rapply(GroupName_terrestrial[3], function(x) base::gsub("Arctic_fox", "Arctic Fox", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[3] <- lapply(rapply(GroupName_terrestrial[3], function(x) base::gsub("Arctic_hare", "Arctic hare", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[4] <- lapply(rapply(GroupName_terrestrial[4], function(x) base::gsub("Long-tailed_jaeger", "Long-tailed jaeger", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[4] <- lapply(rapply(GroupName_terrestrial[4], function(x) base::gsub("Snowy_owl", "Snowy owl", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[4] <- lapply(rapply(GroupName_terrestrial[4], function(x) base::gsub("Arctic_fox", "Arctic fox", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[4] <- lapply(rapply(GroupName_terrestrial[4], function(x) base::gsub("Arctic_hare", "Arctic hare", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[4] <- lapply(rapply(GroupName_terrestrial[4], function(x) base::gsub("Collared_lemming", "Lemmings", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Rough-legged_hawk", "RLHA", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Long-tailed_jaeger", "LTJA", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Snowy_owl", "Snowy_Owl", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Grizzly_bear", "Grizzli_Bear", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Red_fox", "Red_fox", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Arctic_fox", "Arctic_Fox", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Ermine", "Weasels", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Collared_lemming", "Collared_lemmings", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Brown_lemmings", "Brown_lemming", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Peregrine_falcon", "Peregrine_falcon", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Parasitic_jaeger", "Parasitic_jaegers", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Long-tailed_jaeger", "Long_tailed_jaegers", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Rough-legged_hawk", "Rough_legged_hawks", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Snowy_owl", "Snowy_owls", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Collared_lemming", "Collared_lemmings", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Snow_goose", "Snow_geese", x), how = "list"), base::as.data.frame)

# Same operations on terrestrial networks
terrestrial_ntw <- purrr::map2(terrestrial_ntw, GroupName_terrestrial, ~ merge(.x , .y, by.x = "species_from", by.y = "original_name", all.y = TRUE, sort = FALSE))

# Reorder and rename the dataframes
terrestrial_ntw <- lapply(terrestrial_ntw, function(x) {
	 	   as.data.frame(x) |>
		   dplyr::select(c("scientific_name", "species_to", "energy_flow")) |>
		   dplyr::rename(species_from = "scientific_name")
})

terrestrial_ntw <- purrr::map2(terrestrial_ntw, GroupName_terrestrial, ~ merge(.x, .y, by.x = "species_to", by.y = "ID", all = TRUE, sort = FALSE))

terrestrial_ntw <- lapply(terrestrial_ntw, function(x) {
		   dplyr::select(x, c("species_from", "scientific_name", "energy_flow")) |>
		   dplyr::rename(species_to = "scientific_name") |>
		   na.omit()
})

terrestrial_ntw <- lapply(terrestrial_ntw, function(x) {
		   x <- x[which(x$energy_flow > 0),]
		   return(x)
}) 

DIET[c(110,111,112,113,114,115,116)] <- terrestrial_ntw

# Get the networks metadata (ecosystem info) into a list
Ecopath_models <- split(Ecopath_models, seq(nrow(Ecopath_models)))

# Only get the Ecopath_models information that relate to element of the DIET list that are not empty dataframes
Ecopath_models <- Ecopath_models[sapply(DIET, nrow) > 0]

# Only get the networks that are not empty
DIET <- DIET[sapply(DIET, nrow) > 0]

# Add the models name and habitat_type
DIET <- purrr::map2(DIET, Ecopath_models, ~ cbind(.x, .y))

# Unlist the dataframes to one dataframe
# 1624 inter, 1594 unique inter
inter_table <- do.call("rbind", DIET) |>
		dplyr::rename(model_name = "Model name", habitat_type = "Habitat type") |>
		dplyr::select("model_name", "species_from", "species_to", "energy_flow", "habitat_type")

# Write the list as a .Rdata file
saveRDS(inter_table, file = "data/inter_table.RDS")
