# Load the Ecopath models available on their site
h <- RCurl::basicTextGatherer()
RCurl::curlPerform(url = 'http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_3.php', writefunction=h$update)

data <- XML::xmlTreeParse(h$value(), useInternalNodes = TRUE)
model_list <- plyr::ldply(XML::xmlToList(data), data.frame)

# Load the .csv file that matches my model names to their database model names
# Merge it with the corresponding models
# 64 models
name_match <- read.csv("data/raw/last_name_match.csv") |>
              base::merge(model_list, by.x = "name_from_db",
               by.y = "model.model_name", all.x = TRUE
              ) #82 models, has to keep unique ones

# Verified manually the duplicate one to keep the right one
name_match <- name_match[-c(7), ]

# Load the interactions table
ecopath_metadata <- readRDS("data/intermediate/resolved_inter_table.RDS") |>
                    base::merge(name_match, by.x = "model_name",
                     by.y = "model_name", all.x = TRUE) |>
                    dplyr::select(
                     c("model_name", "model.geographic_extent",
                     "model.model_year", "model.ecosystem_type",
                     "model.currency_units")) |>
                    unique() |>
                    dplyr::rename(
                     geographic_extent = "model.geographic_extent",
                     model_year = "model.model_year",
                     ecosystem_type = "model.ecosystem_type",
                     currency_units = "model.currency_units")

# Add manually some missing informations
ecopath_metadata[which(ecopath_metadata$model_name == "Arctic islands, Alert"), "model_year"] <- "2008"
ecopath_metadata[which(ecopath_metadata$model_name == "Arctic islands, Erkuta"), "model_year"] <- "2008"
ecopath_metadata[which(ecopath_metadata$model_name == "Arctic islands, Herschel"), "model_year"] <- "2008"
ecopath_metadata[which(ecopath_metadata$model_name == "Arctic islands, Nenetsky"), "model_year"] <- "2008"
ecopath_metadata[which(ecopath_metadata$model_name == "Arctic islands, Svalbard"), "model_year"] <- "2008"
ecopath_metadata[which(ecopath_metadata$model_name == "Arctic islands, Zackenberg"), "model_year"] <- "2008"
ecopath_metadata[which(ecopath_metadata$model_name == "Arctic islands, Bylot"), "model_year"] <- "2008"
ecopath_metadata[which(ecopath_metadata$model_name == "Chantuto"), "model_year"] <- "2015"

# Make currency uniform
ecopath_metadata[which(ecopath_metadata$currency_units == "WetWeight"),"currency_units"] <- "Wet weight (t/km^2)"
ecopath_metadata[which(is.na(ecopath_metadata$currency_units)),"currency_units"] <- "Wet weight (t/km^2)"

# Add terrestrial netwokr ecosystem_type
ecopath_metadata[which(is.na(ecopath_metadata$ecosystem_type)), "ecosystem_type"] <- "Tundra"

# Write the list as a .Rdata file
saveRDS(ecopath_metadata, file = "data/intermediate/ecopath_metadata.RDS")
