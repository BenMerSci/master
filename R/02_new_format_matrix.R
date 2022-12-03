# Load the matrices
load("data/raw/ecopath/data/Ecopath_models_modif.Rdata")
Group_name <- readRDS("data/intermediate/Group_name.RDS")
load("data/raw/ecopath/data/DIET.Rdata")
load("data/raw/ecopath/data/Q_vec.Rdata")
load("data/raw/ecopath/data/B_vec.Rdata")
ecobase_biomass <- readRDS("data/raw/ecobase_data/biomasses.RDS")

# Convert terrestrial Arctic networks from kg/km2 to tons/km2
B_vec[110:116] <- lapply(B_vec[110:116], function(x){x/1000})
Q_vec[110:116] <- lapply(Q_vec[110:116], function(x){x/1000})

# Convert terrestrial back from dry weight to wet weight
# Plants were 90% water and other organisms 68%
B_vec[[110]][1:4] <- B_vec[[110]][1:4]/0.10
B_vec[[110]][5:length(B_vec[[110]])] <- B_vec[[110]][5:length(B_vec[[110]])]/0.32
B_vec[[111]][1:5] <- B_vec[[111]][1:5]/0.10
B_vec[[111]][6:length(B_vec[[111]])] <- B_vec[[111]][6:length(B_vec[[111]])]/0.32
B_vec[[112]][1:5] <- B_vec[[112]][1:5]/0.10
B_vec[[112]][6:length(B_vec[[112]])] <- B_vec[[112]][6:length(B_vec[[112]])]/0.32
B_vec[[113]][1:5] <- B_vec[[113]][1:5]/0.10
B_vec[[113]][6:length(B_vec[[113]])] <- B_vec[[113]][6:length(B_vec[[113]])]/0.32
B_vec[[114]][1:5] <- B_vec[[114]][1:5]/0.10
B_vec[[114]][6:length(B_vec[[114]])] <- B_vec[[114]][6:length(B_vec[[114]])]/0.32
B_vec[[115]][1:4] <- B_vec[[115]][1:4]/0.10
B_vec[[115]][5:length(B_vec[[115]])] <- B_vec[[115]][5:length(B_vec[[115]])]/0.32
B_vec[[116]][1:5] <- B_vec[[116]][1:5]/0.10
B_vec[[116]][6:length(B_vec[[116]])] <- B_vec[[116]][6:length(B_vec[[116]])]/0.32

# Change the comas in matrices by dots
ecobase_matrice <- list.files(path="./data/raw/ecobase_data", pattern = "(matrix)+(.csv)", full.names = T) |>
    purrr::map(~read.csv(., check.names = FALSE))

ecobase_matrice <- ecobase_matrice |>
      purrr::map(function(x){
          apply(x, 2, function(x) as.numeric(gsub(pattern = ",", replacement = ".", x)))
      })

# Append ecobase_biomass to B_vec
B_vec <- c(B_vec, ecobase_biomass)
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
# Removed 4 for the already computed matrices from Ecobase
for (i in seq_len(length(DIET)-4)) {

  D <- DIET[[i]] # Diet matrix
  B <- as.numeric(B_vec[[i]]) # Species biomass (t/km^2)
  Q <- as.numeric(Q_vec[[i]]) # Species consumption (t/km^2)

  # Outflows matrix (predation)
  DIET[[i]] <- (D %*% diag(Q) %*% diag(B))
  # To remove the column names of last networks
  colnames(DIET[[i]]) <- seq_len(ncol(DIET[[i]]))
}

# Add back the 4 matrices
DIET[c(16,17,18,19)] <- ecobase_matrice

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

# Remove cannibalism from these networks
interactions <- purrr::map(interactions, function(x) {
          x <- x[which(x$prey != x$predator), ]
          return(x)
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

# Remove interactions from terrestrial with Arthropoda
# Have to remove every interactions of a predator that consume
# Arthropoda because of model 4 where we need the total biomass
# sum of the prey of a predator, and we didn't have a good
# relation for going back from dry weight to wet weight
# Names of terrestrial predators to remove that eats "Arthropoda"
arthropoda_predators <- c("Waterfowl","Ptarmigan","Passerines","Shorebirds","Jaegers","Stercorarius longicaudus","Gulls","Plectrophenax nivalis")

inter_table <- inter_table[-which(inter_table$predator %in% arthropoda_predators),]

# Save the data
saveRDS(inter_table, file = "data/intermediate/inter_table.RDS")
