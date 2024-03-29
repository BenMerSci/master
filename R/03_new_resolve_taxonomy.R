# Load the interactions table
inter_table <- readRDS("data/intermediate/inter_table.RDS")

# Get a unique vector of the species names
prelem_sp_list <- tibble::enframe(x = unique(c(inter_table$prey, inter_table$predator)),
 name = NULL, value = "original_name")
#write.csv(prelem_sp_list, "data/intermediate/new/sp_list.csv")

# Temporary store names that make the function bug
temp <- prelem_sp_list[which(prelem_sp_list$original_name %in% c("Litopenaeus","Nereidae","Medium cephalopod")),"original_name"]
index <- which(prelem_sp_list$original_name %in% c("Litopenaeus","Nereidae","Medium cephalopod"))
prelem_sp_list[which(prelem_sp_list$original_name %in% c("Litopenaeus","Nereidae","Medium cephalopod")),"original_name"] <- NA

 # Resolve the names
# If file of resolved_names doesn't exist it creates it, otherwise it loads it
if(!file.exists("data/intermediate/resolved_names.RDS")) {
# Used traitdataform instead of taxize because the
# function is a wrapper around gnr_resolve
# to get the accepted name if it's a synonym
    resolved_names <- traitdataform::get_gbif_taxonomy(
                        prelem_sp_list$original_name, resolve_synonyms = TRUE
                      ) |>
                      tibble::as_tibble() |>
                      dplyr::select(c("verbatimScientificName","scientificName"))
# Replace the NA names
    resolved_names[index, "verbatimScientificName"] <- temp
        saveRDS(resolved_names, "data/intermediate/resolved_names.RDS")
} else{
    resolved_names <-  readRDS("data/intermediate/resolved_names.RDS")
  }

# Manually replace names that had no matches
resolved_names[which(is.na(resolved_names$scientificName)), "scientificName"] <- resolved_names[which(is.na(resolved_names$scientificName)),"verbatimScientificName"]

# Rename columns
resolved_names <- dplyr::rename(resolved_names,
                    original = "verbatimScientificName",
                    resolved = "scientificName"
                  )

# Check which names have change to confirm
resolved_names[which(resolved_names$original != resolved_names$resolved), ]

# Replace one
resolved_names[which(resolved_names$original == "Crustacea and Molluscs"), "resolved"] <- "Crustacea and Molluscs"

# Merge into the table
resolved_inter_table <- base::merge(inter_table, resolved_names, by.x = "prey",
                          by.y = "original", all.x = TRUE) |>
                        dplyr::select(c("model_name", "resolved", "predator",
                          "biomass_flow", "biomass_prey",
                          "biomass_predator", "degree_predator")) |>
                        dplyr::rename(prey = "resolved") |>
                        base::merge(resolved_names, by.x = "predator",
                          by.y = "original", all.x = TRUE) |>
                        dplyr::select("model_name", "prey", "resolved",
                          "biomass_flow", "biomass_prey",
                          "biomass_predator", "degree_predator") |>
                        dplyr::rename(predator = "resolved")

# Save it back into inter_table
saveRDS(resolved_inter_table,
 file = "data/intermediate/resolved_inter_table.RDS"
)
