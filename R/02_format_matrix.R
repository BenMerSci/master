# Script to format the matrices into a list of pairwise interactions

# Load the matrices
load("data/raw/ecopath/data/Ecopath_models_modif.Rdata")
GroupName <- readRDS("data/intermediate/GroupName.RDS")
load("data/raw/ecopath/data/DIET.Rdata")
load("data/raw/ecopath/data/Q_vec.Rdata")
load("data/raw/ecopath/data/B_vec.Rdata")
load("data/raw/ecopath/data/P_vec.Rdata")

pred_mat <- list()
cons_mat <- list()

# Multiplying species consumption with their preys relative importance
# to get the actual flow from the prey.
for(i in 1:length(DIET)){
    
  D <- DIET[[i]] # Diet matrix
  B <- as.numeric(B_vec[[i]]) # Species biomass (t/km^2)
  P <- as.numeric(P_vec[[i]]) # Species production (t/km^2)
  Q <- as.numeric(Q_vec[[i]]) # Species consumption (t/km^2)
  
  e = P/Q 
  e[e=="NaN"] = 0
  e[e=="Inf"] = 0

  # Outflows matrix (predation)
  pred_mat[[i]] <- -t(D%*%diag(Q)%*%diag(B))
  # Inflows matrix (consumption)
  cons_mat[[i]] <- -t(pred_mat[[i]]*e)

 pred_mat[[i]] <- -t(pred_mat[[i]])
}

# Wide to long format
pred_mat <- lapply(pred_mat, function(x) {
		base::as.data.frame(x) |> 
		tibble::rownames_to_column("X") |>
		tidyr::pivot_longer(!X, names_to = "predator", values_to = "pred_flow") |>
		dplyr::rename(prey = "X")
})
cons_mat <- lapply(cons_mat, function(x) {
		base::as.data.frame(x) |> 
		tibble::rownames_to_column("X") |>
		tidyr::pivot_longer(!X, names_to = "predator", values_to = "cons_flow") |>
		dplyr::rename(prey = "X")
})

# Remove the letter X in some of the number IDs
# Done that after substracting 1 to the other networks, because the networks with X in the IDs were indexed starting from 1 and not from 2 like the previous one
pred_mat <- lapply(rapply(pred_mat, function(x) base::gsub("V", "", x), how = "list"), base::as.data.frame)
cons_mat <- lapply(rapply(cons_mat, function(x) base::gsub("V", "", x), how = "list"), base::as.data.frame)

# Format the biomass table to match the names
B_vec <- lapply(B_vec, function(x) base::as.data.frame(x))
B_vec <- purrr::map2(GroupName, B_vec, ~cbind(.x, .y)) |>
	 lapply(function(x) {
	 dplyr::select(x, c("scientific_name","x")) |>
	 dplyr::rename(biomass = "x") |>
	na.omit()
	 })

# Loading the names to join them into the flow matrices
GroupName <- lapply(GroupName, function (x) { 
	     	base::as.data.frame(x) |>
	     	tibble::rownames_to_column("ID") |>
	     	na.omit()
	     })

GroupName_terrestrial <- GroupName[c(110,111,112,113,114,115,116)]

GroupName <- lapply(GroupName, function(x) dplyr::select(x, c("ID", "scientific_name")))

# Save the arctic terrestrial networks
terrestrial_ntw_pred <- pred_mat[c(110,111,112,113,114,115,116)]
terrestrial_ntw_cons <- cons_mat[c(110,111,112,113,114,115,116)]

# Join the names by ID into the flow matrices for "prey"
# Both next methods work, either with baseR or Tidyverse
#flux_mat <- purrr::map2(flux_mat, GroupName, ~ dplyr::right_join(.x, .y, by = c("prey" = "ID")))
pred_mat <- purrr::map2(pred_mat, GroupName, ~ merge(.x, .y, by.x = "prey", by.y = "ID", all.y = TRUE, sort = FALSE))
cons_mat <- purrr::map2(cons_mat, GroupName, ~ merge(.x, .y, by.x = "prey", by.y = "ID", all.y = TRUE, sort = FALSE))

# Reorder and rename the dataframes
pred_mat <- lapply(pred_mat, function(x) {
	as.data.frame(x) |>
	dplyr::select(c("scientific_name","predator","pred_flow")) |>
	dplyr::rename(prey = "scientific_name")
})
cons_mat <- lapply(cons_mat, function(x) {
	as.data.frame(x) |>
	dplyr::select(c("scientific_name","predator","cons_flow")) |>
	dplyr::rename(prey = "scientific_name")
})
# Join again the names by ID into the flow matrices for "predator"
pred_mat <- purrr::map2(pred_mat, GroupName, ~ merge(.x, .y, by.x = "predator", by.y = "ID", all = TRUE, sort = FALSE))
cons_mat <- purrr::map2(cons_mat, GroupName, ~ merge(.x, .y, by.x = "predator", by.y = "ID", all = TRUE, sort = FALSE))

# Reorder and rename the dataframes
pred_mat <- lapply(pred_mat, function(x) {
	dplyr::select(x, c("prey","scientific_name","pred_flow")) |>
	dplyr::rename(predator = "scientific_name") |>
	na.omit()
})
cons_mat <- lapply(cons_mat, function(x) {
	dplyr::select(x, c("prey","scientific_name","cons_flow")) |>
	dplyr::rename(predator = "scientific_name") |>
	na.omit()
})
# Keep only the flow that are > 0
pred_mat <- lapply(pred_mat, function(x) {
	x <- x[which(x$pred_flow > 0),]
	return(x)
})
cons_mat <- lapply(cons_mat, function(x) {
	x <- x[which(x$cons_flow > 0),]
	return(x)
})
# Change names in terrestrial networks
#GroupName_terrestrial[[5]] <- rbind(GroupName_terrestrial[[5]], data.frame(ID = c("11","19","24"), original_name = c("Tundra_voles","Wolverine","Peregrine_falcon"), scientific_name = c("Microtus oeconomus","Gulo gulo","Falco peregrinus")))
#GroupName_terrestrial[[6]] <- rbind(GroupName_terrestrial[[6]], data.frame(ID = c("7","11","12","13"), original_name = c("Brown_lemmings","Glaucus_gulls","Stoats", "Arctic_Foxes"), scientific_name = c("Lemmus trimucronatus","Larus hyperboreus","Mustela erminea","Vulpes lagopus")))

GroupName_terrestrial[1] <- lapply(rapply(GroupName_terrestrial[1], function(x) base::gsub("Long-tailed_jaeger", "Long-tailed Jaegers", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[1] <- lapply(rapply(GroupName_terrestrial[1], function(x) base::gsub("Arctic_wolve", "Arctic wolves", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[1] <- lapply(rapply(GroupName_terrestrial[1], function(x) base::gsub("Arctic_hare", "Arctic hare", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[1] <- lapply(rapply(GroupName_terrestrial[1], function(x) base::gsub("Collared_lemming", "Collared lemmings", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[1] <- lapply(rapply(GroupName_terrestrial[1], function(x) base::gsub("Snow_bunting", "Passerines", x), how = "list"), base::as.data.frame)
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
GroupName_terrestrial[4] <- lapply(rapply(GroupName_terrestrial[4], function(x) base::gsub("Snow_bunting", "Passerines", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Rough-legged_hawk", "RLHA", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Long-tailed_jaeger", "LTJA", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Snowy_owl", "Snowy_Owl", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Grizzly_bear", "Grizzli_Bear", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Arctic_fox", "Arctic_Fox", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Ermine", "Weasels", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Collared_lemming", "Collared_lemmings", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Brown_lemmings", "Brown_lemming", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Muskoxen", "Muskox", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[5] <- lapply(rapply(GroupName_terrestrial[5], function(x) base::gsub("Peregrine_falco", "Peregrine_falcon", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Peregrine_falcon", "Peregrine_falcon", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Parasitic_jaeger", "Parasitic_jaegers", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Long-tailed_jaeger", "Long_tailed_jaegers", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Rough-legged_hawk", "Rough_legged_hawks", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Snowy_owl", "Snowy_owls", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Collared_lemming", "Collared_lemmings", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Snow_goose", "Snow_geese", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Brown_lemming", "Brown_lemmings", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Gulls", "Glaucus_gulls", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Weasel", "Stoats", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[6] <- lapply(rapply(GroupName_terrestrial[6], function(x) base::gsub("Arctic_Fox", "Arctic_Foxes", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[7] <- lapply(rapply(GroupName_terrestrial[7], function(x) base::gsub("Voles", "oles", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[7] <- lapply(rapply(GroupName_terrestrial[7], function(x) base::gsub("Arctic_hare", "Arctic hare", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[7] <- lapply(rapply(GroupName_terrestrial[7], function(x) base::gsub("Arctic_fox", "Arctic Fox", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[7] <- lapply(rapply(GroupName_terrestrial[7], function(x) base::gsub("Red_fox", "Red fox", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[7] <- lapply(rapply(GroupName_terrestrial[7], function(x) base::gsub("Rough-legged_hawk", "Rough legged hawk", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[7] <- lapply(rapply(GroupName_terrestrial[7], function(x) base::gsub("Snowy_owl", "Snowy owl", x), how = "list"), base::as.data.frame)
GroupName_terrestrial[7] <- lapply(rapply(GroupName_terrestrial[7], function(x) base::gsub("Brown_bear", "Brown bears", x), how = "list"), base::as.data.frame)

# Same operations on terrestrial networks
terrestrial_ntw_pred <- purrr::map2(terrestrial_ntw_pred, GroupName_terrestrial, ~ merge(.x , .y, by.x = "prey", by.y = "original_name", all.y = TRUE, sort = FALSE))
terrestrial_ntw_cons <- purrr::map2(terrestrial_ntw_cons, GroupName_terrestrial, ~ merge(.x , .y, by.x = "prey", by.y = "original_name", all.y = TRUE, sort = FALSE))

# Reorder and rename the dataframes
terrestrial_ntw_pred <- lapply(terrestrial_ntw_pred, function(x) {
	 	   as.data.frame(x) |>
		   dplyr::select(c("scientific_name", "predator", "pred_flow")) |>
		   dplyr::rename(prey = "scientific_name")
})
terrestrial_ntw_cons <- lapply(terrestrial_ntw_cons, function(x) {
	 	   as.data.frame(x) |>
		   dplyr::select(c("scientific_name", "predator", "cons_flow")) |>
		   dplyr::rename(prey = "scientific_name")
})

terrestrial_ntw_pred <- purrr::map2(terrestrial_ntw_pred, GroupName_terrestrial, ~ merge(.x, .y, by.x = "predator", by.y = "ID", all = TRUE, sort = FALSE))
terrestrial_ntw_cons <- purrr::map2(terrestrial_ntw_cons, GroupName_terrestrial, ~ merge(.x, .y, by.x = "predator", by.y = "ID", all = TRUE, sort = FALSE))

terrestrial_ntw_pred <- lapply(terrestrial_ntw_pred, function(x) {
		   dplyr::select(x, c("prey", "scientific_name", "pred_flow")) |>
		   dplyr::rename(predator = "scientific_name") |>
		   na.omit()
})
terrestrial_ntw_cons <- lapply(terrestrial_ntw_cons, function(x) {
		   dplyr::select(x, c("prey", "scientific_name", "cons_flow")) |>
		   dplyr::rename(predator = "scientific_name") |>
		   na.omit()
})

terrestrial_ntw_pred <- lapply(terrestrial_ntw_pred, function(x) {
		   x <- x[which(x$pred_flow > 0),]
		   return(x)
}) 
terrestrial_ntw_cons <- lapply(terrestrial_ntw_cons, function(x) {
		   x <- x[which(x$cons_flow > 0),]
		   return(x)
}) 


pred_mat[c(110,111,112,113,114,115,116)] <- terrestrial_ntw_pred
cons_mat[c(110,111,112,113,114,115,116)] <- terrestrial_ntw_cons

# Get the networks metadata (ecosystem info) into a list
Ecopath_models <- split(Ecopath_models, seq(nrow(Ecopath_models)))

# Only get the Ecopath_models information that relate to element of the flux_mat list that are not empty dataframes
Ecopath_models <- Ecopath_models[sapply(pred_mat, nrow) > 0]

# Only get the consumption and biomass data that relate to element of the flux_mat list that are not empty dataframes
#Q_vec <- Q_vec[sapply(flux_mat, nrow) > 0]
B_vec <- B_vec[sapply(pred_mat, nrow) > 0]
# Only get the networks that are not empty
pred_mat <- pred_mat[sapply(pred_mat, nrow) > 0] |>
	lapply(function(x) {
		x$pred_flow <- as.numeric(x$pred_flow)
		return(x)
	})
cons_mat <- cons_mat[sapply(cons_mat, nrow) > 0] |>
	lapply(function(x) {
		x$cons_flow <- as.numeric(x$cons_flow)
		x <- x[,"cons_flow"]
		return(x)
	})

# Bind the two flow matrices together
flux_mat <- purrr::map2(pred_mat, cons_mat, ~ cbind(.x, .y))
flux_mat <- lapply(flux_mat, function(x) dplyr::rename(x, cons_flow = ".y"))

# Add the models name and habitat_type
flux_mat <- purrr::map2(flux_mat, Ecopath_models, ~ cbind(.x, .y))

inter_table <- purrr::map2(flux_mat, B_vec, ~ merge(.x, .y, by.x = "prey", by.y = "scientific_name", all.x = TRUE)) |>
		lapply(function(x) dplyr::rename(x, biomass_prey = "biomass")) |>
		purrr::map2(B_vec, ~ merge(., .y, by.x = "predator", by.y = "scientific_name", all.x = TRUE)) |>
		lapply(function(x) dplyr::rename(x, biomass_pred = "biomass"))

inter_table <- do.call("rbind", inter_table) |>
		dplyr::rename(model_name = "Model name") |>
		dplyr::select("model_name", "prey", "predator", "pred_flow", "cons_flow", "biomass_prey","biomass_pred")


# Write the list as a .Rdata file
saveRDS(inter_table, file = "data/intermediate/inter_table.RDS")

