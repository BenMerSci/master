# Load the Ecopath models available on their site
h = RCurl::basicTextGatherer()
RCurl::curlPerform(url = 'http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_3.php',writefunction=h$update)

data <- XML::xmlTreeParse(h$value(),useInternalNodes=TRUE)
model_list <- plyr::ldply(XML::xmlToList(data),data.frame)

# Load the .csv file that matches my model names to their database model names
# Merge it with the corresponding models
name_match <- read.csv("data/name_match.csv") |>
		dplyr::left_join(model_list, by = c("name_from_db" = "model.model_name")) |>
		unique(

# Load the interactions table
inter_table <- readRDS("data/inter_table.RDS")









