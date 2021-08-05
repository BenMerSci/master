# Load the Ecopath models available on their site
h = RCurl::basicTextGatherer()
RCurl::curlPerform(url = 'http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_3.php',writefunction=h$update)

data <- XML::xmlTreeParse(h$value(),useInternalNodes=TRUE)
model_list <- plyr::ldply(XML::xmlToList(data),data.frame)

# Load the .csv file that matches my model names to their database model names
# Merge it with the corresponding models
# 64 models
name_match <- read.csv("data/name_match.csv") |>
		base::merge(model_list, by.x = "name_from_db", by.y = "model.model_name", all.x = TRUE) #82 models, has to keep unique ones

name_match <- name_match[-c(11,15,17,18,19,22,23,36,37,49,50,55,56,57,60,61,62,69),]

# Load the interactions table
inter_table <- readRDS("data/inter_table.RDS") |>
		base::merge(name_match, by.x = "model_name", by.y = "model_name", all.x = TRUE) |>
		dplyr::select(c("model_name","species_from","species_to","energy_flow","habitat_type",
				"model.geographic_extent","model.ecosystem_type","model.currency_units",
				"model.model_year","model.depth_min","model.depth_max","model.currency_units_custom",
				"model.depth_mean","model.temperature_mean","model.temperature_min","model.temperature_max"))

# Add manually some missing informations
inter_table[which(inter_table$model_name == "Arctic islands, Alert"), "model.model_year"] <- "2008"
inter_table[which(inter_table$model_name == "Arctic islands, Erkuta"), "model.model_year"] <- "2008"
inter_table[which(inter_table$model_name == "Arctic islands, Herschel"), "model.model_year"] <- "2008"
inter_table[which(inter_table$model_name == "Arctic islands, Nenetsky"), "model.model_year"] <- "2008"
inter_table[which(inter_table$model_name == "Arctic islands, Svalbard"), "model.model_year"] <- "2008"
inter_table[which(inter_table$model_name == "Arctic islands, Zackenberg"), "model.model_year"] <- "2008"

# Write the list as a .Rdata file
saveRDS(inter_table, file = "data/inter_table.RDS")