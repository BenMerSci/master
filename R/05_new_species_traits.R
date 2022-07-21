# Load the interaction table to make a unique species list
resolved_inter_table <- readRDS("data/intermediate/resolved_inter_table.RDS")


species_list <- tibble::enframe(unique(c(resolved_inter_table$prey,
                 resolved_inter_table$predator)),
                  name = NULL, value = "scientific_name")

# Get the GBIF IDs for each species
species_list$gbif_id <- apply(species_list, 1, function(x) {
                         taxize::get_gbifid(x, ask = TRUE, messages = TRUE)[1]
                        })

# Get hierarchy for each species from their gbif_id
organism_class <- lapply(tibble::deframe(species_list[, "gbif_id"]), function(x) {
                         taxize::classification(x, db = "gbif")}) |>
                  lapply(function(x) x[[1]]) |>
                  lapply(function(x) if (length(x) < 3) {
                         x <- data.frame(NA)
                       } else{
                          data.frame(matrix(x[which(x$rank %in%
                          c("class", "family")), "name"], nrow = 1, ncol = 2))
                  })
organism_class <- do.call(plyr::"rbind.fill", organism_class)
organism_class <- dplyr::select(organism_class, c("X1", "X2")) |>
                  dplyr::rename(class = "X1", family = "X2")

# Bind the classes to the species list
species_list <- cbind(species_list, organism_class) |>
                dplyr::mutate(length_min = NA, length_mean = NA,
                 length_max = NA, a_min = NA, a_mean = NA, a_max = NA,
                 b_min = NA, b_mean = NA, b_max = NA, bodymass_min = NA,
                 bodymass_mean = NA, bodymass_max = NA)

# Find species/groups with missing class/family info
#missing <- species_list[which(is.na(species_list$class)), ]
species_list[grep("plankton", species_list$scientific_name), c("class","family")] <- "Plankton"
species_list[which(species_list$scientific_name == "Mollusca"), c("class")] <- "Mollusca"
species_list[grep("fish", species_list$scientific_name), c("class")] <- "Actinopterygii" 
species_list[which(species_list$scientific_name == "Flying fish"), c("class","family")] <- c("Actinopterygii","Exocoetidae")
species_list[which(species_list$scientific_name %in% c("Zoobenthos","Macrozoobenthos","Meiofauna","Heterotrophic benthos")), c("class","family")] <- c("Benthos","Benthos")
species_list[which(species_list$scientific_name == "Microcrustacean"), c("class")] <- c("Crustacea")
species_list[which(species_list$scientific_name %in% c("Benthic producer","Phytobenthos","Shrubs","Mosses","Forbs","Grasses","Macrophytes","Lichens")), c("class","family")] <- c("Producer","Producer")
species_list[which(species_list$scientific_name %in% c("Benthic deposit feeders","Benthic suspension feeders")), c("class","family")] <- c("Invertebrate","Invertebrate")
species_list[which(species_list$scientific_name %in% c("Mesopelagic micronekton","Epipelagic micronekton")), c("class","family")] <- c("Nekton","Nekton")
species_list[which(species_list$scientific_name == "Shorebirds"), c("class","family")] <- c("Aves","Scolopacidae")
species_list[which(species_list$scientific_name == "Passerines"), c("class")] <- c("Aves")
species_list[which(species_list$scientific_name == "Waterfowl"), c("class","family")] <- c("Aves","Anatidae")
species_list[which(species_list$scientific_name == "Geese"), c("class","family")] <- c("Aves","Anatidae")
species_list[which(species_list$scientific_name == "Gulls"), c("class","family")] <- c("Aves","Laridae")
species_list[which(species_list$scientific_name == "Ptarmigan"), c("class","family")] <- c("Aves","Phasianidae")
species_list[which(species_list$scientific_name == "Seabirds"), c("class")] <- c("Aves")
species_list[which(species_list$scientific_name %in% c("Planktivorous haplochromines","Predatory haplochromines","Benthivorous haplochromines","Cichlids")), c("class","family")] <- c("Actinopterygii","Cichlidae")
species_list[which(species_list$scientific_name == "Lemmings"), c("class","family")] <- c("Mammalia","Cricetidae")
species_list[which(species_list$scientific_name == "Croaker"), c("class","family")] <- c("Actinopterygii","Sciaenidae")
species_list[which(species_list$scientific_name == "Goby"), c("class","family")] <- c("Actinopterygii","Gobiidae")
species_list[which(species_list$scientific_name == "Shrimps"), c("class","family")] <- c("Malacostraca","Penaeidae")
species_list[which(species_list$scientific_name == "Snook"), c("class","family")] <- c("Actinopterygii","Centropomidae")
species_list[which(species_list$scientific_name == "Crabs"), c("class","family")] <- c("Malacostraca","Portunidae")
species_list[which(species_list$scientific_name == "Snout"), c("class")] <- c("Actinopterygii")
species_list[which(species_list$scientific_name == "Arthropoda"), c("class")] <- c("Arthropoda")
species_list[which(species_list$scientific_name == "Large cetacea"), c("class","family")] <- c("Mammalia","Delphinidae")
species_list[which(species_list$scientific_name == "Small cetacea"), c("class")] <- c("Mammalia")
species_list[which(species_list$scientific_name == "Benthic predators"), c("class")] <- c("Polychaeta")
species_list[which(species_list$scientific_name == "Copepoda"), c("class")] <- c("Hexanauplia")
species_list[which(species_list$scientific_name == "Large shark"), c("class","family")] <- c("Chondrichthyes","Lamnidae")
species_list[which(species_list$scientific_name == "Sheephead"), c("class","family")] <- c("Actinopterygii","Sparidae")


############################
# Find species bodymasses #
############################
# Validate the fish names
#fish <- species_list[which(species_list$class %in%
#         c("Actinopterygii", "Elasmobranchii", "Sarcopterygii")), ]
#validated_fish <- data.frame(cbind(fish$scientific_name,
#                   rfishbase::validate_names(fish$scientific_name))) |>
#                  dplyr::rename(original = "X1", validated = "X2")
## Get the length-weight relationships
#fish_LW <- rfishbase::length_weight(validated_fish$validated)
#fish_LW_ls <- base::split(fish_LW, f = fish_LW$Species)
#fish_LW_ls <- purrr::map(fish_LW_ls, ~ dplyr::mutate(., LengthMin = as.numeric(.$LengthMin),
#                                              LengthMax = as.numeric(.$LengthMax)) |>
#                                       dplyr::mutate(bodymass_min = .$a * .$LengthMin^.$b,
#                                              bodymass_max = .$a * .$LengthMax^.$b) |>
#                                       dplyr::select(c("Species","bodymass_min","bodymass_max")) |>
#                                       dplyr::group_by(Species) |>
#                                       dplyr::summarise(bodymass_min = mean(bodymass_min, na.rm = TRUE),
#                                        bodymass_max = mean(bodymass_max, na.rm = TRUE))                    
#                        )
#
#fish_LW_ls <- do.call("rbind", fish_LW_ls)
#species_list$bodymass_min <- fish_LW_ls$bodymass_min[match(species_list$scientific_name,fish_LW_ls$Species)]
#species_list$bodymass_max <- fish_LW_ls$bodymass_max[match(species_list$scientific_name,fish_LW_ls$Species)]

# Check which species are NAs for bodymass
#missing_mass <- species_list[which(is.na(species_list$bodymass_min & species_list$bodymass_max)),]

################################
## Manual entry of bodymasses ##
################################
# IMPORTANT: for fish without common length, I took max-(max-min/2)
# For every fish, where possible, the data were taken from Fishbase with the Bayesian L-W,
# since this is the relationship which is the most available
# (The Bayesian L-W relationships can be found at the bottom of the Fishbase page for a species),
# bodymass_min are computed using the smallest length found with the lower bounds for a and b
# bodymass_max are computed using the largest length found with the highest bounds for a and b
# bodymass_mean are computed using common length if available, if not found somewhere else with source and using the average a and b values
# For some species, bodymass_max is the max reported in litterature from the fishbase page (will be specified if the case)
# Did that because some of them didn't really make sense

# Acanthostracion quadricornis (https://fishbase.in/summary/Acanthostracion-quadricornis.html)
species_list[which(species_list$scientific_name == "Acanthostracion quadricornis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(17,20,55,0.02370,0.04074,0.07002,2.54,2.70,2.86)

# Archosargus rhomboidalis (https://fishbase.in/summary/Archosargus-rhomboidalis.html)
species_list[which(species_list$scientific_name == "Archosargus rhomboidalis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(8,20,33,0.01729,0.02042,0.02411,2.93,2.98,3.03)

# Bagrus docmak0.00933 (https://fishbase.in/summary/Bagrus-docmak.html)
species_list[which(species_list$scientific_name == "Bagrus docmak"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20,62,127,0.00730,0.00933,0.01193,2.94,3.00,3.06)

# Bairdiella chrysoura (https://fishbase.in/summary/Bairdiella-chrysoura.html)
species_list[which(species_list$scientific_name == "Bairdiella chrysoura"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(9.3,20,30,0.00768,0.01148,0.01716,2.96,3.08,3.20)

# Carcharhinus melanopterus (https://fishbase.in/summary/Carcharhinus-melanopterus.html)
species_list[which(species_list$scientific_name == "Carcharhinus melanopterus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(91,120,200,0.00252,0.00468,0.00868,2.88,3.03,3.18)

# Chaetodipterus faber (https://fishbase.in/summary/Chaetodipterus-faber.html) max weight reported
species_list[which(species_list$scientific_name == "Chaetodipterus faber"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max","bodymass_max")] <-
                                   c(12,50,91,0.02599,0.03388,0.04418,2.86,2.94,3.02,9000)

# Citharichthys spilopterus (https://fishbase.in/summary/Citharichthys-spilopterus.html)
# length_min based on same genus species at maturity
species_list[which(species_list$scientific_name == "Citharichthys spilopterus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(3,15,21,0.00501,0.00617,0.00758,3.08,3.13,3.18)

# Clarias gariepinus (https://fishbase.in/summary/Clarias-gariepinus.html)
species_list[which(species_list$scientific_name == "Clarias gariepinus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(30.8,90,170,0.00671,0.00813,0.00985,2.93,2.98,3.03)

# Copadichromis azureus (https://fishbase.in/summary/Copadichromis-azureus.html)
# length_min is the mean length at maturity for species of same genus (https://fishbase.in/Reproduction/MaturityList_genus.php)
# length_max is the length_max of Copadichromis azureus
species_list[which(species_list$scientific_name == "Copadichromis azureus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(mean(c(10.3,10.6,13.0)),NA,14.6,0.00687,0.01479,0.03186,2.79,2.97,3.15)

# Dicentrarchus labrax (https://fishbase.in/summary/Dicentrarchus-labrax.html)
species_list[which(species_list$scientific_name == "Dicentrarchus labrax"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(23,50,103,0.00767,0.00933,0.01136,2.96,3.02,3.08)

# Engraulicypris sardella (https://fishbase.in/summary/Engraulicypris-sardella.html)
species_list[which(species_list$scientific_name == "Engraulicypris sardella"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(NA,10,13,0.00254,0.00447,0.00785,2.85,3.01,3.17)

# Haemulon plumierii (https://fishbase.in/summary/Haemulon-plumierii.html)
species_list[which(species_list$scientific_name == "Haemulon plumierii"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(11,30,53,0.01168,0.01413,0.01709,2.95,2.99,3.03)
                            
# Haplochromis angustifrons (https://fishbase.in/summary/Haplochromis-angustifrons.html)
species_list[which(species_list$scientific_name == "Haplochromis angustifrons"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(NA,NA,9,0.00244,0.01000,0.04107,2.81,3.04,3.27)

# Haplochromis nigripinnis (https://fishbase.in/summary/Haplochromis-nigripinnis.html)
species_list[which(species_list$scientific_name == "Haplochromis nigripinnis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(NA,NA,6.8,0.00708,0.01445,0.02949,2.80,2.97,3.14)

# Haplochromis squamipinnis (https://fishbase.in/summary/Haplochromis-squamipinnis.html)
species_list[which(species_list$scientific_name == "Haplochromis squamipinnis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(NA,NA,20.2,0.00687,0.01479,0.03186,2.79,2.97,3.15)

# Harengula jaguana (https://fishbase.in/summary/Harengula-jaguana.html)
species_list[which(species_list$scientific_name == "Harengula jaguana"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(8,12,21.2,0.00766,0.00891,0.01037,3.02,3.06,3.10)

# Hydrocynus forskahlii (https://fishbase.in/summary/Hydrocynus-forskahlii.html)
species_list[which(species_list$scientific_name == "Hydrocynus forskahlii"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20,49,78,0.00384,0.00759,0.01500,2.97,3.15,3.33)

# Hydrocynus vittatus (https://fishbase.in/summary/Hydrocynus-vittatus.html)
species_list[which(species_list$scientific_name == "Hydrocynus vittatus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(39.8,74,105,0.00870,0.01096,0.01382,2.95,3.01,3.07)

# Lagodon rhomboides (https://fishbase.in/summary/Lagodon-rhomboides.html)
species_list[which(species_list$scientific_name == "Lagodon rhomboides"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(9,18,40,0.01189,0.01349,0.01530,3.01,3.04,3.07)

# Lates niloticus (https://fishbase.in/summary/Lates-niloticus.html)
species_list[which(species_list$scientific_name == "Lates niloticus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(53,100,200,0.00761,0.00977,0.01254,2.93,3.00,3.07)

# Lates stappersii (https://fishbase.in/summary/Lates-stappersii.html)
species_list[which(species_list$scientific_name == "Lates stappersii"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(19.3,27,45,0.00628,0.00813,0.01053,2.92,2.99,3.06)

# Limnothrissa miodon (https://fishbase.in/summary/Limnothrissa-miodon.html)
species_list[which(species_list$scientific_name == "Limnothrissa miodon"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(6.8,10,17,0.00405,0.00724,0.01297,2.84,2.99, 3.14)

# Lutjanus griseus (https://fishbase.in/summary/Lutjanus-griseus.html)
species_list[which(species_list$scientific_name == "Lutjanus griseus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(21,40,89,0.01296,0.01479,0.01687,2.95,2.98,3.01)

# Mugil curema
species_list[which(species_list$scientific_name == "Mugil curema"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(21,30,91,0.00981,0.01148,0.01343,2.92,2.95,2.98)

# Opisthonema oglinum (https://fishbase.in/summary/Opisthonema-oglinum.html)
species_list[which(species_list$scientific_name == "Opisthonema oglinum"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(11,20,38,0.00729,0.00832,0.00949,3.00,3.04,3.08)

# Opsanus beta (https://fishbase.in/summary/Opsanus-beta.html)
species_list[which(species_list$scientific_name == "Opsanus beta"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(10,20,30,0.00795,0.01288,0.02088,2.92,3.06,3.20)

# Opsaridium microcephalum (https://fishbase.in/summary/Opsaridium-microcephalum.html)
species_list[which(species_list$scientific_name == "Opsaridium microcephalum"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(23.7,28.85,34,0.00313,0.00708,0.01602,2.86,3.05,3.24)

# Oreochromis leucostictus (https://fishbase.in/summary/Oreochromis-leucostictus.html)
species_list[which(species_list$scientific_name == "Oreochromis leucostictus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(6,22,36.3,0.00737,0.01349,0.02470,2.83,2.98,3.13)

# Oreochromis niloticus (https://fishbase.in/summary/Oreochromis-niloticus.html)
species_list[which(species_list$scientific_name == "Oreochromis niloticus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(6,28,60,0.01586,0.01905,0.02290,2.93,2.97,3.01)

# Orthopristis chrysoptera (https://fishbase.in/summary/Orthopristis-chrysoptera.html)
# length_min (maturity) from  Orthopristis rubra (samge genus)
species_list[which(species_list$scientific_name == "Orthopristis chrysoptera"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(15.6,30,46,0.01122,0.01778,0.02818,2.87,3.01,3.15)

# Platichthys flesus (https://fishbase.in/summary/Platichthys-flesus.html)
species_list[which(species_list$scientific_name == "Platichthys flesus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(14,50,60,0.00701,0.00794,0.00901,3.02,3.06,3.10)

# Pomatoschistus microps (https://fishbase.in/summary/Pomatoschistus-microps.html)
species_list[which(species_list$scientific_name == "Pomatoschistus microps"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(3,6,9,0.00407,0.00776,0.01480,2.91,3.07,3.23)

# Protopterus aethiopicus (https://fishbase.in/summary/Protopterus-aethiopicus.html)
species_list[which(species_list$scientific_name == "Protopterus aethiopicus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(70,130,200,0.00156,0.00389,0.00972,2.88,3.10,3.32)

# Rastrineobola argentea (https://www.fishbase.de/summary/Rastrineobola-argentea.html)
species_list[which(species_list$scientific_name == "Rastrineobola argentea"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(4.4,6.2,8,0.00337,0.00676,0.01355,2.88,3.06,3.24)

# Rhamphochromis longiceps (https://www.fishbase.de/summary/Rhamphochromis-longiceps.html)
species_list[which(species_list$scientific_name == "Rhamphochromis longiceps"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(13,20.5,28,0.00244,0.01000,0.04107,2.81,3.04,3.27)

# Stolothrissa tanganicae (https://www.fishbase.de/summary/Stolothrissa-tanganicae.html)
species_list[which(species_list$scientific_name == "Stolothrissa tanganicae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(6.5,7,10,0.00230,0.00550,0.01313,2.90,3.11,3.32)

# Synodontis schall (https://www.fishbase.de/summary/Synodontis-schall.html)
species_list[which(species_list$scientific_name == "Synodontis schall"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(12,24.5,37,0.01143,0.01445,0.01829,2.89,2.95,3.01)
                                   
# Other fish1 -> 
# Trisopterus luscus","Pleuronectes platessa","Clupea harengus","Sprattus sprattus","Solea solea","Pomatoschistus microps
# smallest parameters is for Pomatoschistus microps
# Averaged some of them for the mean
# max parameters is for 
species_list[which(species_list$scientific_name == "Other fish1"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(3,35,100,0.00407,0.00759,0.00903,2.91,3.06,3.10)
#rfishbase::length_weight(c("Trisopterus luscus","Pleuronectes platessa","Clupea harengus","Sprattus sprattus","Solea solea","Pomatoschistus microps"), server = "fishbase") |>
#               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#               dplyr::select(c("bodymass_min", "bodymass_max")) |>
#               colMeans(na.rm = TRUE)

# Mollusca -> arbitrary
species_list[which(species_list$scientific_name == "Mollusca"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,25.25,50)

# Microcrustacean -> arbitrary
species_list[which(species_list$scientific_name == "Microcrustacean"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,12.75, 25)

# Decapoda -> Penaeidae & brachyura
# Mixed the shrimps and crabs mass.
# bodymass_mean is midpoint between max and min
shrimps <- c(0.7133*(1.2^2.940), 0.0084*(10.8^2.956)) # shrimps
crabs <- rfishbase::length_weight(c("Callinectes danae", "Callinectes boucorti", "Callinectes sapidus"), server = "sealifebase") |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)
decapoda <- rbind(shrimps,crabs) |>
            colMeans()
species_list[which(species_list$scientific_name == "Decapoda"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(15.94638,117.152,218.35769)
            
# Nereidae
# bodymass_min and max were from Nereis pelagic from the table in (doi:10.1017/S0025315409991408)
# bodymass_mean was computed with the inbetween of bodymass_max and bodymass_min
species_list[which(species_list$scientific_name == "Nereidae"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.007,0.3235,0.64)


# Benthic producer -> arbitrary decision (only small benthic prod, seagrass, macrophytes)
# arbitrary
species_list[which(species_list$scientific_name == "Benthic producer"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.05,0.525,1)

# Zooplankton
# Yodzis
species_list[which(species_list$scientific_name == "Zooplankton"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.0001,0.5005,1)

# Phytoplankton
# Yodzis
species_list[which(species_list$scientific_name == "Phytoplankton"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.0001,0.00055,0.001)

# Forbs -> arbitrary
species_list[which(species_list$scientific_name == "Forbs"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,0.75,1)

# Shrubs -> arbitrary
species_list[which(species_list$scientific_name == "Shrubs"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,0.75,1)

# Mosses -> arbitrary
species_list[which(species_list$scientific_name == "Mosses"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,0.75,1)

# Grasses -> arbitrary
species_list[which(species_list$scientific_name == "Grasses"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,0.75,1)

# Eucinostomus
# from Eucinostomus gula
species_list[which(species_list$scientific_name == "Eucinostomus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(11,15,25.5,0.00920,0.01096,0.01307,3.04,3.08,3.12)
#rfishbase::length_weight(c("Eucinostomus gula", "Eucinostomus argenteus")) |>
#               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#               dplyr::select(c("bodymass_min", "bodymass_max")) |>
#               colMeans(na.rm = TRUE)

# Haplochromis nigripinnis, fishbase bayesian L-W relationships
# (https://www.fishbase.de/summary/Haplochromis-nigripinnis.html)
species_list[which(species_list$scientific_name == "Haplochromis nigripinnis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(6.8,6.8,6.8,0.00708,0.01445,0.02949,2.80,2.97,3.14)

# Zoobenthos -> Chaoborus spp., Copepods, Oligochaetes (arbitrary)
species_list[which(species_list$scientific_name == "Zoobenthos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.05,0.525,1)

# Haplochromis angustifrons, fishbase bayesian L-W relationships
# (https://www.fishbase.de/summary/Haplochromis-angustifrons.html)
species_list[which(species_list$scientific_name == "Haplochromis angustifrons"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(9,9,9,0.00244,0.01000,0.04107,2.81,3.04,3.27)

# Haplochromis squamipinnis, fishbase bayesian L-W relationships
# (https://www.fishbase.de/summary/Haplochromis-squamipinnis.html)
species_list[which(species_list$scientific_name == "Haplochromis squamipinnis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20.2,20.2,20.2,0.00687,0.01479,0.03186,2.79,2.97,3.15)

# Phytobenthos -> Macroalgae, Chlorophytes (green algae) and Pheophytes (brown algae), Rhodophytes (red algae) -> arbitrary
species_list[which(species_list$scientific_name == "Phytobenthos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.005,0.5025,1) 

# Benthic deposit feeders (Pectinaria koreni, Macoma balthica and Owenia fusiformis)
# Used 10.1017/S0025315409991408, for the only Terebellida present (for Pectinaria koreni)
species_list[which(species_list$scientific_name == "Benthic deposit feeders"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.05,4.025,10) 

# Benthic suspension feeders (Cerastoderma edule and Abra alba), mean of both with L-W relationships on sealifebase
# Based of Cerastoderma edule
species_list[which(species_list$scientific_name == "Benthic suspension feeders"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(1.1,3.5,5.6,1.1570,1.1570,1.1570,2.26,2.26,2.26)

# Caridina nilotica
# https://www.sciencedirect.com/science/article/pii/S1474706512000824
# Used the bodymass of control
species_list[which(species_list$scientific_name == "Caridina nilotica"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.01,0.076,0.142)

# Macrozoobenthos -> arbitrary, went 0.5-2 just to be bigger than Zoobenthos
species_list[which(species_list$scientific_name == "Macrozoobenthos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,1.25,2)

# Microtus oeconomus
# (https://animaldiversity.org/accounts/Microtus_oeconomus/)
species_list[which(species_list$scientific_name == "Microtus oeconomus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(25, 50, 80)

# Geese (Anser brachyrhynchus, Branta leucopsis)
# Wikipedia (https://en.wikipedia.org/wiki/Pink-footed_goose)(https://en.wikipedia.org/wiki/Barnacle_goose)
species_list[which(species_list$scientific_name == "Geese"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(1210,2160,3400)

# Lemmings (Lemmus trimucronatus, Dicrostonyx groenlandicus)
# Used the data of Lemmus trimucronatus
species_list[which(species_list$scientific_name == "Lemmings"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(30, 63, 113)

# Lemmus trimucronatus
# Mix Wikipedia (https://en.wikipedia.org/wiki/Canadian_lemming) and NatureServe (https://explorer.natureserve.org/Taxon/ELEMENT_GLOBAL.2.105072/Lemmus_trimucronatus)
# Used the same bodymass_min as Dicrostonyx groenlandicus
species_list[which(species_list$scientific_name == "Lemmus trimucronatus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(30, 63, 113)

# Dicrostonyx groenlandicus
# (https://animaldiversity.org/accounts/Dicrostonyx_groenlandicus/)
# bodymass_mean is midpoint between bodymass_max and bodymass_min
species_list[which(species_list$scientific_name == "Dicrostonyx groenlandicus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(30,71,112)

# Mustela erminea
# Mix of (https://animaldiversity.org/accounts/Mustela_erminea/) and Eltontraits and Body mass of late Quaternary mammals (SMITH 2003)
species_list[which(species_list$scientific_name == "Mustela erminea"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(25,70,168.8) 

# Shorebirds
# Calidris bairdii, C. fusciollis, C. melanotos, Pluvialis dominica, P. squatarola, Phalaropus fulicarius
# From the Wikipedia pages of all the species
# Arbitrary bodymass_mean
species_list[which(species_list$scientific_name == "Shorebirds"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(38,100,280)

# Vulpes lagopus
# Wikipedia (https://en.wikipedia.org/wiki/Arctic_fox)
species_list[which(species_list$scientific_name == "Vulpes lagopus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(1400,3200,9400)

# Passerines
# Calcarius lapponicus, Plectrophenax nivalis, Eremophila alpestris, Anthus rubescens
# From the Wikipedia pages of all the species
# bodymass_mean is the midpoint between bodymass_max and bodymass_min
species_list[which(species_list$scientific_name == "Passerines"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(22,35,48)

# Waterfowl, based the smallest and largest: Anas crecca, Cygnus columbianus
# (https://en.wikipedia.org/wiki/Eurasian_teal)(https://en.wikipedia.org/wiki/Tundra_swan)
# bodymass_mean was chosen arbitrary based on all the species
species_list[which(species_list$scientific_name == "Waterfowl"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(340,2500,9600)

# Gulls (Larus hyperboreus, L. argentatus)
# Wikkipedia (https://en.wikipedia.org/wiki/Glaucous_gull)(https://en.wikipedia.org/wiki/European_herring_gull)
species_list[which(species_list$scientific_name == "Gulls"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(710,1332.083,2700)

# Plectrophenax nivalis
# Wikipedia (https://en.wikipedia.org/wiki/Snow_bunting) (https://fr.wikipedia.org/wiki/Bruant_des_neiges)
species_list[which(species_list$scientific_name == "Plectrophenax nivalis"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(26,35,50) # Wikipedia (https://fr.wikipedia.org/wiki/Bruant_des_neiges)

# Anser caerulescens atlantica
# Wikipedia (https://en.wikipedia.org/wiki/Snow_goose)
# bodymass_mean is the midpoint between bodymass_max and bodymass_min
species_list[which(species_list$scientific_name == "Anser caerulescens atlanticus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(2050,2375,2700) 

# Ptarmigan
# Wikipedia(https://en.wikipedia.org/wiki/Willow_ptarmigan)(https://en.wikipedia.org/wiki/Rock_ptarmigan)
species_list[which(species_list$scientific_name == "Ptarmigan"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(430,580,810) 

# Lepus arcticus
# Wikipedia(https://en.wikipedia.org/wiki/Arctic_hare)
# bodymass_mean is the midpoint between bodymass_max and bodymass_min
species_list[which(species_list$scientific_name == "Lepus arcticus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(2500,4750,7000)

# Ovibos moschatus
# Wikipedia(https://en.wikipedia.org/wiki/Muskox)
species_list[which(species_list$scientific_name == "Ovibos moschatus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(180000,285000,410000)

# Rangifer tarandus
# Wikipedia(https://fr.wikipedia.org/wiki/Rangifer_tarandus)
species_list[which(species_list$scientific_name == "Rangifer tarandus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(80000,142000,182000)

# Shrimps, Penaeus setiferus, P. aztecus
# Used only Penaeus setiferus since it seems to encompass P. aztecus mass also
species_list[which(species_list$scientific_name == "Shrimps"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(1.7,10.8,19,0.0084,0.0084,0.0084,2.956,2.956,2.956)

# Anchoa
# Based on Anchoa mitchilli (https://fishbase.in/summary/Anchoa-mitchilli.html)
species_list[which(species_list$scientific_name == "Anchoa"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(4,5.9,10,0.00418,0.00537,0.00690,3.12,3.16,3.20)
##anchoa <- rfishbase::fb_tbl("species") |>
 # dplyr::filter(Genus == "Anchoa") |>
 # dplyr::mutate(sp_name = paste(Genus, Species, sep = " ")) |>
 # dplyr::select(c("sp_name"))
#rfishbase::validate_names(anchoa) |>
#rfishbase::length_weight() |> dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#               dplyr::select(c("bodymass_min", "bodymass_max")) |>
#               colMeans(na.rm = TRUE)

# Croaker
# Min based on Bairdiella chrysoura
# Mean based on Bairdiella ronchus
# Max based on Micropogonias undulatus
species_list[which(species_list$scientific_name == "Croaker"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(9.3,25,55,0.00768,0.00871,0.01283,2.96,3.11,3.30)
#rfishbase::length_weight(c("Bairdiella chrysura", "Bairdiella ronchus", "Micropogon undulatus")) |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Gerreidae
# Min based on Diapterus rhombeus
# Mean based on Eucinostomus melanopterus
# Max based on Diapterus auratus
species_list[which(species_list$scientific_name == "Gerreidae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(13,23,42.8,0.01020,0.01023,0.01339,2.98,3.08,3.13)
#rfishbase::length_weight(c("Eucinostomus melanopterus", "Diapterus rhombeus", "Diapterus auratus", "Eugerres plumieri")) |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Crustacea -> Clivanarius vittatus, decapod, amphipod
# Used 10.1017/S0025315409991408 for lowest (amphipoda) and the lowest of decpoda
# bodymass_mean is midpoint between both
species_list[which(species_list$scientific_name == "Crustacea"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.02,7.980448,15.94638) # To confirm

# Goby
# Min based on Ctenogobius boleosoma mean
# Mean based on Bathygobius soporator min
# Max based on Bathygobius soporator
species_list[which(species_list$scientific_name == "Goby"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(4,8.2,15,0.00661,0.00651,0.00884,3.03,3.06,3.14)
#rfishbase::length_weight(c("Bathygobius soporator", "Gobionellus boleosoma")) |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Scombridae
# Min from Alepes djedaba
# mean and max from Euthynnus affinis
species_list[which(species_list$scientific_name == "Scombridae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(16.3,60,100,0.01197,0.01000,0.01164,2.92,3.05,3.09)
#rfishbase::length_weight(c("Rastrelliger kanagurta", "Euthynnus affinis", "Alepes djedaba")) |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Small pelagic fish, Hilsa kelee
species_list[which(species_list$scientific_name == "Small pelagic fish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(15,16.5,35,0.00640,0.01023,0.01636,2.89,3.02,3.15)
#rfishbase::length_weight(c("Hilsa kelee")) |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Demersal fish
# Min from Johnius dussumieri
# Mean from Pomadasys maculatus
# Max from Otolithes ruber
species_list[which(species_list$scientific_name == "Demersal fish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(11,30,90,0.00646,0.01738,0.00935,3.05,2.93,3.10)
#rfishbase::length_weight(c("Pomadasys maculatus", "Otolithes ruber", "Johnius dussumieri", "Johniops sina")) |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Planktivorous haplochromines, https://www.biodiversitylibrary.org/part/119071 for the names
# Took the Haplochromis lividus from Haplochromis lividus, Haplochromis phytophagus,Haplochromis obliquidens,Haplochromis nuchisquamulatus
# (https://fishbase.in/summary/Haplochromis-lividus.html)
# Used Bayesian L-W relationships from fishbase
# For the bodymass_max, used the asymptotic weight in the article (35)
species_list[which(species_list$scientific_name == "Planktivorous haplochromines"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max","bodymass_max")] <-
                                   c(9,9,NA,0.00244,0.01000,NA,2.81,3.04,NA,35)

# Oreochromis -> Oreochromis esculentus from the article (major species)
# (https://fishbase.in/summary/Oreochromis-esculentus.html)
species_list[which(species_list$scientific_name == "Oreochromis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(17,31,50,0.00833,0.01445,0.02507,2.87,3.01,3.15)
#rfishbase::length_weight(c("Oreochromis esculentus")) |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Predatory haplochromines, https://www.biodiversitylibrary.org/part/119071
# Used Bayesian L-W relationships from fishbase (took the smallest of the five species)
# For the bodymass_max, used the asymptotic weight in the article (205)
# Based on Haplochromis squamulatus (https://fishbase.in/summary/Haplochromis-squamulatus.html)
species_list[which(species_list$scientific_name == "Predatory haplochromines"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max","bodymass_max")] <-
                                   c(19.8,19.8,NA,0.00244,0.01000,NA,2.81,3.04,NA,205)


# Crassostrea virginica (https://sealifebase.ca/summary/Crassostrea-virginica.html)
# Used the length-weight relationship from sealifebase and length from  WORMS https://www.marinespecies.org/aphia.php?p=taxdetails&id=140657#attributes
species_list[which(species_list$scientific_name == "Crassostrea virginica"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(0.2,10,20,0.0210,0.0210,0.0210,2.490,2.490,2.490)

# Crabs, Callinectes danae, C. boucorti, C. sapidus
# Used the largest species(C. sapidus) (https://sealifebase.ca/summary/Callinectes-sapidus.html)
species_list[which(species_list$scientific_name == "Crabs"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(6,13.65,21.3,0.1182,0.1182,0.1182,2.772,2.772,2.772)
#rfishbase::length_weight(c("Callinectes danae", "Callinectes boucorti", "Callinectes sapidus"), server = "sealifebase") |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Meiofauna -> arbitrary
species_list[which(species_list$scientific_name == "Meiofauna"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.005,0.2525,0.5)

# Snout -> Synodontis and mormyrus
# Used Synodontis victoriae (https://fishbase.in/summary/Synodontis-victoriae.html)
# lower-bound from L-W relationship, higher from article
species_list[which(species_list$scientific_name == "Snout"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max","bodymass_max")] <-
                                   c(8,21.5,NA,0.00625,0.01230,NA,2.77,2.94,NA,650)
#Snout <- rfishbase::fb_tbl("species") |>
#  dplyr::filter(Genus %in% c("Synodontis","Mormyrus")) |>
#  dplyr::mutate(sp_name = paste(Genus, Species, sep = " ")) |>
#  dplyr::select(c("sp_name")) #|>
#rfishbase::length_weight(Snout$sp_name) |> dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#               dplyr::select(c("bodymass_min", "bodymass_max")) |>
#               colMeans(na.rm = TRUE)

# Snook -> Centropomus undecimalis, C. paralellus, C. poeyi
# Min from parallelus
# Mean and max from undecimalis
species_list[which(species_list$scientific_name == "Snook"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(25,50,140,0.00637,0.00692,0.00859,3.01,3.04,3.09)
#rfishbase::length_weight(c("Centropomus undecimalis", "Centropomus paralellus", "Centropomus poeyi")) |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Benthivorous haplochromines
# Used Haplochromis tyrianthinus (https://fishbase.in/summary/Haplochromis-tyrianthinus.html)
# Bayesian length-weight relationshops
# Took the asymptotic weight in the article
species_list[which(species_list$scientific_name == "Benthivorous haplochromines"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max","bodymass_max")] <-
                                   c(10.5,10.5,NA,0.00244,0.01000,NA,2.81,3.04,NA,40)

# Periphyton -> arbitrary
species_list[which(species_list$scientific_name == "Periphyton"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.05,0.275,0.5)

# Macrophytes -> arbitrary
species_list[which(species_list$scientific_name == "Macrophytes"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,0.75,1)

# Bivalvia -> Corbicula africana, Caelatura mossarnbicensis, Aspatharia wahlbergi and Mutela dubia
# Used the same as Mollusca
species_list[which(species_list$scientific_name == "Bivalvia"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,25.25,50)
               #rfishbase::length_weight(c("Corbicula africana", "Caelatura mossarnbicensis", "Aspatharia wahlbergi", "Mutela dubia"), server = "sealifebase") |>
               #dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               #dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               #dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               #dplyr::select(c("bodymass_min", "bodymass_max")) |>
               #colMeans(na.rm = TRUE)

# Chaoborus edulis
# Used the same as Zoobenthos, since Chaoborus is part of this group
species_list[which(species_list$scientific_name == "Chaoborus edulis"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.05,0.525,1)

# Large catfish
# Based of Bagrus docmak (https://fishbase.in/summary/Bagrus-docmak.html)
species_list[which(species_list$scientific_name == "Large catfish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20,62,127,0.00730,0.00933,0.01193,2.94,3.00,3.06)
#clarias <- rfishbase::fb_tbl("species") |>
#  dplyr::filter(Genus %in% c("Clarias")) |>
#  dplyr::mutate(sp_name = paste(Genus, Species, sep = " ")) |>
#  dplyr::select(c("sp_name")) 
#rfishbase::length_weight(c("Bagrus docmak",clarias$sp_name)) |> dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#               dplyr::select(c("bodymass_min", "bodymass_max")) |>
#               colMeans(na.rm = TRUE)

# Small catfish
# Based of Ariopsis felis (https://fishbase.in/summary/Ariopsis-felis.html)
species_list[which(species_list$scientific_name == "Small catfish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(13,25,70,0.00490,0.00724,0.01070,3.07,3.19,3.31)
#rfishbase::length_weight(c("Ariopsis felis","Cathorops melanopus")) |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Neomysis integer -> used the shrimps part of the Decapoda group
# bodymass_mean is the midpoint
species_list[which(species_list$scientific_name == "Neomysis integer"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(1.219172,5.374444,9.529715)

# Heterotrophic benthos -> same as Macrozoobenthos
species_list[which(species_list$scientific_name == "Heterotrophic benthos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,1.25,2)

# Crustacea and Molluscs
# Used Scylla serrata (https://sealifebase.ca/summary/Scylla-serrata.html)
species_list[which(species_list$scientific_name == "Crustacea and Molluscs"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(1,12,20.6,0.4580,0.4580,0.4580,2.653,2.653,2.653)
#rfishbase::length_weight(c("Polynices mamilla","Murex ramosus","Scylla serrata","Eumarcia paupercula","Modiolus philippinarum"), server= "sealifebase") |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Palaemon longirostris -> used the shrimps part of the Decapoda group
species_list[which(species_list$scientific_name == "Palaemon longirostris"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(1.219172,5.374444,9.529715)


# Benthic predators -> Nereis diversicolor, Nephtys hombergii
species_list[which(species_list$scientific_name == "Benthic predators"), c("bodymass_min","bodymass_mean","bodymass_max")] <-c(0.089,0.3345,0.58)
#rfishbase::length_weight(c("Nereis diversicolor", "Nephtys hombergii"), server = "sealifebase") |>
#               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#               dplyr::select(c("bodymass_min", "bodymass_max")) |>
#               colMeans(na.rm = TRUE)

# Crangon crangon (https://sealifebase.ca/summary/Crangon-crangon.html)
species_list[which(species_list$scientific_name == "Crangon crangon"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(0.7,4.35,8,0.0169,0.0169,0.0169,2.647,2.647,2.647)
#rfishbase::length_weight("Crangon crangon", server = "sealifebase") |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Lichens
species_list[which(species_list$scientific_name == "Lichens"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,0.75,1) # To confirm

# Engraulicypris sardella (https://www.fishbase.de/summary/Engraulicypris-sardella.html)
# Arbitrary length_min
species_list[which(species_list$scientific_name == "Engraulicypris sardella"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(3,10,13,0.00254,0.00447,0.00785,2.85,3.01,3.17)

# Copepoda, putting same as Zookplankton
species_list[which(species_list$scientific_name == "Copepoda"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.0001,0.50005,1)

# Stercorarius longicaudus
# (https://en.wikipedia.org/wiki/Long-tailed_jaeger)
species_list[which(species_list$scientific_name == "Stercorarius longicaudus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(230,337,444) # WIkipedia(https://en.wikipedia.org/wiki/Long-tailed_jaeger)

# Arthropoda -> Chironomidae, Muscidae, Aranea, Lepidoptera, Ichneumonidae, Carabidae
# Arbitrary
species_list[which(species_list$scientific_name == "Arthropoda"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,0.75,1)

# Alepisaurus -> A. minutus and A. ferox
# Based on Alepisaurus ferox (https://www.fishbase.de/summary/Alepisaurus-ferox.html)
species_list[which(species_list$scientific_name == "Alepisaurus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(46,150,215,0.00180,0.00389,0.00842,2.94,3.12,3.30)
#rfishbase::length_weight(c("Alepisaurus minutus","Alepisaurus ferox"), server = "fishbase") |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Synodontis zambezensis 
# Bayesian L-W from fishbase (https://www.fishbase.ca/summary/Synodontis-zambezensis.html)
species_list[which(species_list$scientific_name == "Synodontis zambezensis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(15,29,43,0.00509,0.01072,0.02256,2.84,3.03,3.22)

# Limnothrissa miodon
# Bayesian L-W from fishbase (https://www.fishbase.ca/summary/Limnothrissa-miodon.html)
species_list[which(species_list$scientific_name == "Limnothrissa miodon"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(6.8,10,17,0.00405,0.00724,0.01297,2.84,2.99,3.14)

# Cichlids -> Tilapia rendalli, Serranochromis codringtoni, Serranochromis macrocephalus, Oreochromis mortimeri
# Based on Tilapia rendalli (https://www.fishbase.de/summary/Coptodon-rendalli.html)
species_list[which(species_list$scientific_name == "Cichlids"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(17.7,31.35,45,0.01399,0.01660,0.01968,2.93,2.98,3.03)
#rfishbase::validate_names(c("Tilapia rendalli", "Serranochromis codringtoni", "Serranochromis macrocephalus", "Oreochromis mortimeri")) |>
#rfishbase::length_weight() |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Stolothrissa tanganicae
# Bayesian L-W from fishbase (https://www.fishbase.de/summary/Stolothrissa-tanganicae.html)
species_list[which(species_list$scientific_name == "Stolothrissa tanganicae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(6.5,7,10,0.00230,0.00550,0.01313,2.90,3.11,3.32)

# Benthic fish -> Labeo horie, Barbus bynni, Citharinus citharis and  Distichodus nefasch
# Used Labeobarbus bynni (https://www.fishbase.de/summary/Labeobarbus-bynni.html)
# Used same genus to get length_min (maturity) ->30.4
species_list[which(species_list$scientific_name == "Benthic fish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(30.4,56.2,82,0.00432,0.00955,0.02113,2.83,3.01,3.19)
#rfishbase::validate_names(c("Labeo horie", "Barbus bynni", "Citharinus citharus", "Distichodus nefasch")) |>
#rfishbase::length_weight() |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Lates stappersii -> Bayesian L-W from fishbase (Bayesian L-W from fishbase)
# (https://www.fishbase.de/summary/Lates-stappersii.html)
species_list[which(species_list$scientific_name == "Lates stappersii"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20,32.5,45,0.00628,0.00813,0.01053,2.92,2.99,3.06)

# Lates
# Used Lates niloticus (https://www.fishbase.de/summary/Lates-niloticus.html)
species_list[which(species_list$scientific_name == "Lates"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(53,100,200,0.00761,0.00977,0.01254,2.93,3.00,3.07)
#rfishbase::validate_names(c("Lates mariae", "Lates microlepis", "Lates angustifrons","Lates niloticus", "Lates longispinis")) |>
#rfishbase::length_weight() |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Copadichromis azureus -> Bayesian L-W from fishbase (https://fishbase.in/summary/Copadichromis-azureus.html)
species_list[which(species_list$scientific_name == "Copadichromis azureus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(14.6,14.6,14.6,0.00687,0.01479,0.03186,2.79,2.97,3.15)
               #rfishbase::validate_names(c("Copadichromis azureus")) |>
               #rfishbase::length_weight() |>
               #dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               #dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               #dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               #dplyr::select(c("bodymass_min", "bodymass_max")) |>
               #colMeans(na.rm = TRUE)

# Arius
# Based on Arius felis (https://www.fishbase.de/summary/Ariopsis-felis.html)
species_list[which(species_list$scientific_name == "Arius"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(13,25,70,0.00490,0.00724,0.01070,3.07,3.19,3.31)
#rfishbase::validate_names(c("Arius felis", "Arius melanopus")) |>
#rfishbase::length_weight() |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Bubo scandiacus (https://en.wikipedia.org/wiki/Snowy_owl)
species_list[which(species_list$scientific_name == "Bubo scandiacus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(1300,1872.5,2.951)

# Buteo lagopus (https://en.wikipedia.org/wiki/Rough-legged_buzzard)
species_list[which(species_list$scientific_name == "Buteo lagopus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(600,1051.75,1660)

#  Canis lupus arctos (https://fr.wikipedia.org/wiki/Loup_arctique)
# bodymass_min -> (https://www.museedelhistoire.ca/cmc/exhibitions/hist/cae/col1111bf.html)
species_list[which(species_list$scientific_name == "Canis lupus arctos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(19000, 45000, 80000)

# Carangidae
# Caranx crysos for length_min
# Caranx latus for length_mean
# Caranx hippos for length_max
species_list[which(species_list$scientific_name == "Carangidae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(27.4,60,124,0.01457,0.01738,0.02068,2.92,2.92,2.97)
#rfishbase::validate_names(c("Caranx amblyrhynchus", "Caranx hippos", "Caranx latus", "Caranx crysos")) |>
#rfishbase::length_weight() |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Diplotaxodon -> Bayesian L-W from fishbase
# Based on Diplotaxodon limnothrissa for Bayesian L-W:
# Diplotaxodon macrops, Diplotaxodon limnothrissa, Diplotaxodon greenwoodi, Diplotaxodon ecclesi, Diplotaxodon argenteus, Diplotaxodon apogon, Diplotaxodon aeneus
species_list[which(species_list$scientific_name == "Diplotaxodon"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(15.4,15.4,15.4,0.00687,0.01479,0.03186,2.79,2.97,3.15)

# Falco -> Falco peregrinus, F. rusticolus (https://en.wikipedia.org/wiki/Peregrine_falcon)(https://en.wikipedia.org/wiki/Gyrfalcon)
species_list[which(species_list$scientific_name == "Falco"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(330, 1200, 2100)

# Falco peregrinus (https://en.wikipedia.org/wiki/Peregrine_falcon)
species_list[which(species_list$scientific_name == "Falco peregrinus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(330,882.5,1500)

# Gulo gulo (https://en.wikipedia.org/wiki/Wolverine)
species_list[which(species_list$scientific_name == "Gulo gulo"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(7000,16125,27500)

# Jaegers -> Stercorarius longicaudus, S. parasiticus (https://en.wikipedia.org/wiki/Parasitic_jaeger)(https://en.wikipedia.org/wiki/Long-tailed_jaeger)
species_list[which(species_list$scientific_name == "Jaegers"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(230,406,650)

# Larus hyperboreus (https://en.wikipedia.org/wiki/Glaucous_gull)
species_list[which(species_list$scientific_name == "Larus hyperboreus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(960,1450,2700)

# Opsaridium microcephalum (https://www.fishbase.ca/summary/Opsaridium-microcephalum.html)
# bodymass_mean is the midpoint 
species_list[which(species_list$scientific_name == "Opsaridium microcephalum"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(23.7,28.85,34,0.00284,0.00646,0.01468,2.88,3.07,3.26)

# Other fish2
# bodymasss_min from Clupea harengus (https://www.fishbase.de/summary/Clupea-harengus.html)
# bodymass_mean from Trisopterus luscus (https://www.fishbase.ca/summary/Trisopterus-luscus.html)
# bodymass_max from Pleuronectes platessa (https://fishbase.in/summary/Pleuronectes-platessa.html)
species_list[which(species_list$scientific_name == "Other fish2"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(16.7,30,100,0.00485,0.00776,0.00903,3.06,3.11,3.10)
#rfishbase::validate_names(c("Trisopterus luscus", "Pleuronectes platessa", "Clupea harengus", "Sprattus sprattus", "Solea vulgaris", "Pomatoschistus microps")) |>
#rfishbase::length_weight() |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Rhamphochromis longiceps
# Bayesian L-W from fishbase (https://www.fishbase.ca/summary/Rhamphochromis-longiceps.html)
species_list[which(species_list$scientific_name == "Rhamphochromis longiceps"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(13,20.5,28,0.00244,0.01000,0.04107,2.81,3.04,3.27)

# Seabirds
# Calidris alpina,Haemotopus ostrealegus,Numenius aquatus,Fulica atra,Anas crecca,Anas plathyrhynchus,Larus ridibundus,Larus argentus, Podiceps cristatus, Phalocrocorax carbo
# bodymass_min from Calidris alpina
# bodymass_mean is midpoint
# bodymass_max from Phalocrocorax carbo
species_list[which(species_list$scientific_name == "Seabirds"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(48,1874,3700)

# Fishing birds
# bodymass_min from kingfishers(https://en.wikipedia.org/wiki/Belted_kingfisher)
# bodymass_mean is midpoint
# bodymass_max from pelicans (https://fr.wikipedia.org/wiki/P%C3%A9lican_gris)
species_list[which(species_list$scientific_name == "Fishing birds"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(113,3556.5,7000)

# Sheephead
# bodymass_min from Lagodon rhomboides (https://www.fishbase.de/summary/Lagodon-rhomboides.html)
#bodymass_mean and max from Archosargus probatocephalus (https://www.fishbase.de/summary/Archosargus-probatocephalus.html)
species_list[which(species_list$scientific_name == "Sheephead"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(9,35,91,0.01189,0.02188,0.02622,3.01,3.04,3.07)
#rfishbase::validate_names(c("Archosargus probatocephalus", "Lagodon romboides")) |>
#rfishbase::length_weight() |>
#dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
#dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
#dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
#dplyr::select(c("bodymass_min", "bodymass_max")) |>
#colMeans(na.rm = TRUE)

# Stercorarius parasiticus (https://en.wikipedia.org/wiki/Parasitic_jaeger)
species_list[which(species_list$scientific_name == "Stercorarius parasiticus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(300,475,650)

# Ursus arctos (https://en.wikipedia.org/wiki/Brown_bear)
species_list[which(species_list$scientific_name == "Ursus arctos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(135000,166000,217000)

# Vulpes vulpes (https://fr.wikipedia.org/wiki/Renard_roux)
species_list[which(species_list$scientific_name == "Vulpes vulpes"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(2200,8100,14000)


# Compute mass for species from fishbase and sealifebase
species_list[which(!is.na(species_list$length_min)),"bodymass_min"] <- species_list[which(!is.na(species_list$length_min)),"a_min"]*(
       species_list[which(!is.na(species_list$length_min)),"length_min"]^species_list[which(!is.na(species_list$length_min)),"b_min"]
)
species_list[which(!is.na(species_list$length_mean)),"bodymass_mean"] <- species_list[which(!is.na(species_list$length_mean)),"a_mean"]*(
       species_list[which(!is.na(species_list$length_mean)),"length_mean"]^species_list[which(!is.na(species_list$length_mean)),"b_mean"]
)
species_list[which(!is.na(species_list$length_max)),"bodymass_max"] <- species_list[which(!is.na(species_list$length_max)),"a_max"]*(
       species_list[which(!is.na(species_list$length_max)),"length_max"]^species_list[which(!is.na(species_list$length_max)),"b_max"]
)




# Check remaining missing
# Check which species are NAs for bodymass
missing_mass2 <- species_list[which(is.na(species_list$bodymass_min & species_list$bodymass_max & species_list$bodymass_mean)),]

# Add species metabolic classes
species_list[which(species_list$class %in% c("Aves", "Mammalia")), "metabolic_class"] <- "endotherm"
species_list[which(species_list$scientific_name == "Fishing birds"), "metabolic_class"] <- "endotherm"
species_list[which(species_list$family %in% c("Istiophoridae","Xiphiidae","Scombridae")), "metabolic_class"] <- "homeotherm"
species_list[which(is.na(species_list$metabolic_class)),"metabolic_class"] <- "ectotherm"

# Save the file
saveRDS(species_list, "data/intermediate/species_traits.RDS")
