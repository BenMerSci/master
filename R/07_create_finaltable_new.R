# Load the data needed
interactions <- readRDS("data/intermediate/resolved_inter_table.RDS")
traits <- readRDS("data/intermediate/species_traits.RDS")
enviro <- readRDS("data/intermediate/enviro_traits.RDS")

# Merge the tables together
dataset <- merge(interactions, traits, by.x = "prey",
            by.y = "scientific_name", all.x = TRUE) |>
           dplyr::rename(class_prey = "class", family_prey = "family",
            metabolism_prey = metabolic_class, bodymass_min_prey = bodymass_min,
            bodymass_mean_prey = bodymass_mean, bodymass_max_prey = bodymass_max) |>
           dplyr::select(-"gbif_id") |>
           merge(traits, by.x = "predator", by.y = "scientific_name",
            all.x = TRUE) |>
           dplyr::rename(class_predator = "class", family_predator = "family",
            metabolism_predator = metabolic_class, bodymass_min_predator = bodymass_min,
            bodymass_mean_predator = bodymass_mean, bodymass_max_predator = bodymass_max) |>
           dplyr::select(-"gbif_id") |>
           merge(enviro, by.x = "model_name", by.y = "model_name",
            all.x = TRUE) |>
           dplyr::rename(c(flux_units = "currency_units")) |>
           dplyr::select("prey", "predator", "pred_flow",
            "flux_units", "biomass_prey", "bodymass_min_prey",
            "bodymass_mean_prey", "bodymass_max_prey", "class_prey",
            "family_prey", "metabolism_prey", "degree_predator",
            "biomass_predator", "bodymass_min_predator", "bodymass_mean_predator",
            "bodymass_max_predator", "class_predator", "family_predator",
            "metabolism_predator", "model_name", "model_year", "ecosystem_type",
            "habitat_type", "water_temperature", "air_temperature")

# Keeping only wet weight interactions
# not sure about dry wet
# waiting to see if we can convert to back to wet weight
dataset <- dataset |> dplyr::filter(flux_units == "Wet weight (t/km^2)")

# Change species mass from grams to tons
dataset$bodymass_min_prey <- dataset$bodymass_min_prey * 0.000001
dataset$bodymass_mean_prey <- dataset$bodymass_mean_prey * 0.000001
dataset$bodymass_max_prey <- dataset$bodymass_max_prey * 0.000001
dataset$bodymass_min_predator <- dataset$bodymass_min_predator * 0.000001
dataset$bodymass_mean_predator <- dataset$bodymass_mean_predator * 0.000001
dataset$bodymass_max_predator <- dataset$bodymass_max_predator * 0.000001

# Generate unique IDs for each predator
dataset <- dplyr::left_join(dataset, tibble::rownames_to_column(as.data.frame(unique(dataset$predator))), by = c("predator" = "unique(dataset$predator)")) |>
	dplyr::rename(pred_id = "rowname") |>
    dplyr::mutate(pred_id = as.numeric(pred_id)) |>
    dplyr::left_join(tibble::rownames_to_column(as.data.frame(unique(dataset$model_name))), by = c("model_name" = "unique(dataset$model_name)")) |>
    dplyr::rename(foodweb_id = "rowname")

# Change Zooplankton habitat_type to marine_freshwater
dataset$habitat_type <- as.character(dataset$habitat_type)
dataset[which(dataset$predator %in% c("Zooplankton","Cichlids","Bivalvia",
                                      "Macrozoobenthos","Zoobenthos")), "habitat_type"] <- "marine_freshwater"

# Compute predators abundances
dataset <- dataset |>
           dplyr::mutate(abundance_predator =
                          biomass_predator / bodymass_mean_predator)

# Create another predator id by foodweb respectively
# to compute the total biomass consumption of each predator
# by foodweb respectively
dataset <- dplyr::mutate(dataset, predator_by_web = paste(dataset$predator, dataset$model_name, sep = "_")) |>
           dplyr::mutate(pred_id_by_web = as.numeric(as.factor(predator_by_web)))

# Compute the biomass sum of each prey for every predator, respective of network
dataset <- dataset |>
          dplyr::group_by(pred_id_by_web) |>
            dplyr::summarise(sum_biomass_prey = sum(biomass_prey)) |>
              dplyr::left_join(dataset, by = "pred_id_by_web")

# Define predator trophic guilds
## Plankton
dataset[grep("plankton", dataset$predator), "trophic_guild"] <- "Plankton"

## Reptiles
dataset[which(dataset$predator == "Crocodilians"), "trophic_guild"] <- "Reptiles"

## Birds
dataset[which(dataset$class_predator == "Aves"), "trophic_guild"] <- "Non-predatory birds"
dataset[which(dataset$family_predator %in% c("Laridae","Stercorariidae","Accipitridae","Strigidae","Falconidae")), "trophic_guild"] <- "Predatory birds"
dataset[which(dataset$predator %in% c("Seabirds","Fishing birds","Jaegers")), "trophic_guild"] <- "Predatory birds"

## Mammals
dataset[which(dataset$predator %in% c("Mustela erminea")), "trophic_guild"] <- "Small mammal predator"
dataset[which(dataset$predator %in% c("Vulpes vulpes","Gulo gulo","Vulpes lagopus")), "trophic_guild"] <- "Medium mammal predator"
dataset[which(dataset$predator %in% c("Ursus arctos","Canis lupus arctos")), "trophic_guild"] <- "Large mammal predator"
dataset[which(dataset$predator %in% c("Lemmings","Microtus oeconomus","Lemmus trimucronatus","Lepus arcticus","Dicrostonyx groenlandicus")), "trophic_guild"] <- "Small mammal herbivore"
dataset[which(dataset$predator %in% c("Ovibos moschatus","Rangifer tarandus")), "trophic_guild"] <- "Large mammal herbivore"

## Reef-coast associated
dataset[which(dataset$predator %in% c("Snapper Grunts","Selaroides leptolepis","Carangoides malabaricus",
                                     "Sphyraena obtusata","Selar crumenophthalmus","Parastromateus niger",
                                     "Lutjanus griseus","Gobiidae","Amblygaster sirm","Encrasicholina heteroloba",
                                     "Sphoeroides annulatus","Terapon theraps")), "trophic_guild"] <- "Small reef-coast"

dataset[which(dataset$predator %in% c("Chirocentrus dorab","Sphyraena jello","Strongylura leiura","Carangidae",
                                     "Lutjanidae","Chaetodipterus faber","Sheephead")), "trophic_guild"] <- "Medium reef-coast"

## Shark
dataset[which(dataset$predator %in% c("Carcharhinus melanopterus","Demersal shark","Hexanchus griseus","Rays Sharks")), "trophic_guild"] <- "Sharks"

## Pelagic
dataset[which(dataset$predator %in% c("Hemiramphus","Lactarius lactarius","Haplochromis squamipinnis","Rhamphochromis longiceps","Predatory haplochromines","Synodontis schall","Rastrelliger kanagurta","Synodontis zambezensis")), "trophic_guild"] <- "Small pelagic carnivore"
dataset[which(dataset$predator %in% c("Elops affinis","Lepidion lepidion","Scomberomorus commerson","Katsuwonus pelamis","Auxis thazard","Bagrus docmak","Hydrocynus forskahlii","Alepisaurus","Benthic fish","Large scianidae")), "trophic_guild"] <- "Medium pelagic carnivore"
dataset[which(dataset$predator %in% c("Diplotaxodon","Limnothrissa miodon","Stolothrissa tanganicae","Anchoa","Clupeidei","Copadichromis azureus","Haplochromis angustifrons","Haplochromis nigripinnis","Hilsa kelee","Hirundichthys oxycephalus","Oreochromis leucostictus","Oreochromis niloticus","Oreochromis","Planktivore fish","Planktivorous haplochromines","Poeciliidae","Rastrineobola argentea","Rhynchorhamphus malabaricus","Sardinella gibbosa","Small pelagic fish","Thryssa setirostris")), "trophic_guild"] <- "Small pelagic omnivore"
dataset[which(dataset$predator %in% c("Coryphaena hippurus","Scombridae","Chanos chanos","Mugil cephalus","Mugilidae","Otolithes ruber")), "trophic_guild"] <- "Medium pelagic omnivore"

## Demersal
dataset[which(dataset$predator %in% c("Haemulidae","Small demersal fish","Cichlids","Omnivore fish","Gerreidae","Lates stappersii","Dormitator latifrons","Small catfish")), "trophic_guild"] <- "Small demersal omnivore"
dataset[which(dataset$predator %in% c("Clarias gariepinus","Lepturacanthus savala","Snook","Lates","Protopterus aethiopicus")), "trophic_guild"] <- "Medium demersal omnivore"
dataset[which(dataset$predator %in% c("Netuma bilineata","Leiognathus brevirostris","Micropogon undulatus","Eleotridae","Ariopsis guatemalensis","Pleuronectiforms","Cynoglossus zanzibarensis","Carnivore fish","Opsaridium microcephalum","Snout","Alepocephalus rostratus","Benthivorous haplochromines","Benthopelagic fish","Brachirus orientalis","Croaker","Demersal fish2","Engraulicypris sardella","Macrouridae","Opsanus beta")), "trophic_guild"] <- "Small demersal carnivore"
dataset[which(dataset$predator %in% c("Demersal fish","Lophius piscatorius","Mora moro","Phycis blennoides","Cynoponctius coniceps","Synodus sp","Sciaenidae","Atractosteus tropicus","Hydrocynus vittatus","Lates niloticus","Large catfish")), "trophic_guild"] <- "Medium demersal carnivore"

## Shrimps
dataset[which(dataset$predator %in% c("Aristeus antennatus","Caridina nilotica","Macrobrachium","Litopenaeus","Shrimps")), "trophic_guild"] <- "Shrimps"

## Mollusc and crustacea
dataset[which(dataset$predator %in% c("Bivalvia","Crabs","Crassostrea virginica","Crustacea and Molluscs","Crustacea","Grapsid","Microcrustacean","Mollusca")), "trophic_guild"] <- "Mollusc and crustacea"

## Invertebrates
dataset[which(dataset$predator %in% c("Epibenthos","Chaoborus edulis","Heterotrophic benthos","Infauna","Insects","Macrozoobenthos","Meiofauna","Nereidae","Polychaeta","Zoobenthos")), "trophic_guild"] <- "Invertebrates"

## Cephalopods
dataset[which(dataset$predator %in% c("Medium cephalopod","Small cephalopod")), "trophic_guild"] <- "Cephalopods"

# Save the dataset
saveRDS(dataset, "data/clean/new_dataset.RDS")
