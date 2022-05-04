# Load the interactions table
inter_table <- readRDS("data/intermediate/inter_table.RDS")

# Get a unique vector of the species names
prelem_sp_list <- data.frame(unique(c(inter_table$prey,
                   inter_table$predator))) |>
                  dplyr::rename(original_name =
                   "unique.c.inter_table.prey..inter_table.predator..")

# Resolve the names
# If file of resolved_names doesn't exist it creates it, otherwise it loads it
if(!file.exists("data/intermediate/resolved_names.RDS")) {
# Used traitdataform instead of taxize because the
# function is a wrapper around gnr_resolve
# to get the accepted name if it's a synonym
    resolved_names <- traitdataform::get_gbif_taxonomy(
                        prelem_sp_list$original_name, resolve_synonyms = TRUE
                      )
    saveRDS(resolved_names, "data/intermediate/resolved_names.RDS")
} else{
    resolved_names <-  readRDS("data/intermediate/resolved_names.RDS")
  }

# Manually replace names that had no matches
resolved_names[which(is.na(resolved_names$scientificName)), "scientificName"] <- resolved_names[which(is.na(resolved_names$scientificName)),"verbatimScientificName"]

# Select only desired columns
resolved_names <- cbind(
                    resolved_names["verbatimScientificName"],
                    resolved_names["scientificName"]
                  ) |>
                  dplyr::rename(
                    original = "verbatimScientificName",
                    resolved = "scientificName"
                  )

# Merge back the corrected names into inter_table
resolved_inter_table <- base::merge(inter_table, resolved_names, by.x = "prey",
                          by.y = "original", all.x = TRUE) |>
                        dplyr::select(c("model_name", "resolved", "predator",
                          "pred_flow", "cons_flow", "biomass_prey",
                          "biomass_pred", "degrees", "total_flux")) |>
                        dplyr::rename(prey = "resolved") |>
                        base::merge(resolved_names, by.x = "predator",
                          by.y = "original", all.x = TRUE) |>
                        dplyr::select("model_name", "prey", "resolved",
                          "pred_flow", "cons_flow", "biomass_prey",
                          "biomass_pred", "degrees", "total_flux") |>
                        dplyr::rename(predator = "resolved")

# Save it back into inter_table
saveRDS(resolved_inter_table,
 file = "data/intermediate/resolved_inter_table.RDS"
)