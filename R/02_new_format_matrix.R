# Load the matrices
load("data/raw/ecopath/data/Ecopath_models_modif.Rdata")
Group_name <- readRDS("data/intermediate/Group_name.RDS")
load("data/raw/ecopath/data/DIET.Rdata")
load("data/raw/ecopath/data/Q_vec.Rdata")
load("data/raw/ecopath/data/B_vec.Rdata")
biomass_data <- purrr::map(B_vec, ~tibble::enframe(. ,name = "pred_id", value = "biomass"))

# Use the Group_name indices to subset the other object and keep
# only the ones about the network we are using
ntw_indices <- purrr::map_lgl(Group_name, .f = function(x) {
                 sum(complete.cases(x$scientific_name)) > 0
               })

DIET <- DIET[ntw_indices]
B_vec <- B_vec[ntw_indices]
Q_vec <- Q_vec[ntw_indices]
Ecopath_models <- Ecopath_models[ntw_indices, ]
Group_name <- Group_name[ntw_indices]
biomass_data <- biomass_data[ntw_indices]

# First we compute the fluxes for every community
# Calculation taken from Claire Jacquet scripts
for (i in seq_len(length(DIET))) {

  D <- DIET[[i]] # Diet matrix
  B <- as.numeric(B_vec[[i]]) # Species biomass (t/km^2)
  Q <- as.numeric(Q_vec[[i]]) # Species consumption (t/km^2)

  # Outflows matrix (predation)
  DIET[[i]] <- (D %*% diag(Q) %*% diag(B))
  # To remove the column names of last networks
  colnames(DIET[[i]]) <- seq_len(ncol(DIET[[i]]))
}

# Compute the degree (number of preys) for each consumer
# and bind it to Group_name
sp_data <- DIET |>
  purrr::map(~ colSums(. > 0)) |>
   purrr::map(tibble::enframe, name = "pred_id", value = "degree") |>
    purrr::map(~dplyr::mutate(., pred_id = as.integer(pred_id))) |>
     purrr::map2(Group_name, dplyr::bind_cols) |>
      purrr::map2(biomass_data, ~dplyr::left_join(
        .x, .y, by = "pred_id"
      ))

# Format each interaction matrices
## Put species names as col/row names
interactions <- purrr::map2(DIET, sp_data, .f = function(.x, .y) {
  colnames(.x) <- .y$scientific_name
  rownames(.x) <- .y$scientific_name
  .x <- as.data.frame(.x) |>
         tibble::rownames_to_column("prey") |>
          tidyr::pivot_longer(!prey, names_to = "predator",
           values_to = "pred_flow") |>
            dplyr::filter(pred_flow > 0)
  return(.x)
})



interactions <- purrr::map2(interactions, sp_data, .f = function(.x, .y) {
          dplyr::left_join(.x, .y, by = c("predator" = "scientific_name")) |>
          dplyr::select(c("prey","predator","pred_flow","biomass","degree")) |>
          dplyr::rename(biomass_predator = "biomass", degree_predator = "degree") |>
          dplyr::left_join(.y, by = c("prey" = "scientific_name")) |>
          dplyr::select(c("prey","predator","pred_flow","degree_predator","biomass_predator","biomass")) |>
          dplyr::rename(biomass_prey = "biomass")
        })

# Merge community name to each community
interactions <- purrr::map2(asplit(Ecopath_models, 1), interactions, .f = function(.x, .y) {
                 .x <- dplyr::slice(data.frame(t(.x)), rep(1:dplyr::n(), each = nrow(.y)))
                 cbind(.y, .x) |>
                 dplyr::rename(model_name = "Model.name", habitat_type = "Habitat.type")
                })

inter_table <- do.call("rbind", interactions)

# Save the data
saveRDS(inter_table, file = "data/intermediate/inter_table.RDS")

# Andrew's checking
# Check to see if there is a causation between
# not taxonomically resolved and numbers of degrees
#pred_summaries <- dplyr::bind_rows(sp_data, .id = "comm") |>
#                   dplyr::as_tibble() |>
#                    dplyr::mutate(is_resolved = !is.na(scientific_name)) |>
#                     dplyr::group_by(comm, is_resolved) |>
#                      dplyr::summarise(n = dplyr::n(),
#                       mean_deg = median(degree))
#pred_summaries |> dplyr::select(-n) |> tidyr::pivot_wider(names_from = is_resolved, values_from = mean_deg) |> dplyr::arrange(desc(`FALSE`))
#dplyr::bind_rows(sp_data, .id = "comm") |> dplyr::count(original_name)|> dplyr::arrange(dplyr::desc(n)) |> head(15)