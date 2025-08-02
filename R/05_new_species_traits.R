test <- rvest::read_html("https://fishbase.se/summary/Bagrus_docmak.html") |>
  rvest::html_elements(xpath = "//*[contains(@class, 'smallSpace')]")


rfishbase::length_weight("Aristeus antennatus")

get_fishbase_length <- function(species_list) {
  test <- rfishbase::maturity(species_list = species_list) |>
    dplyr::filter(Type1 %in% c("TL", "FL")) |>
    dplyr::summarize(length = mean(LengthMatMin))
}

rfishbase::maturity(species_list = species_list$scientific_name[1:6])


test_length <- get_fishbase_length(species_list$scientific_name[1:6])


fishbase_sp <- species_list |>
  dplyr::filter(class %in% c("Carangidae","Soleidae","Clupeidae","Engraulidae",
                            "Trichiuridae","Chanidae","Eleotridae","Actinopterygii",
                            "Poeciliidae","Scombridae","Clariidae","Cichlidae",
                            "Dipneusti","Gobiidae","Gerreidae","Hemiramphidae",
                            "Mugilidae","Clupeoidei","Leiognathidae",
                            "Chirocentridae","Exocoetidae","Lactariidae",
                            "Ariidae","Chondrichthyes","Lophiidae","Moridae",
                            "Phycidae","Macrouridae","Alepocephalidae","Cyprinidae",
                            "Haemulidae","Lutjanidae","Bagridae","Alepisauridae",
                            "Mochokidae","Terapontidae","Sciaenidae","Latidae",
                            "Alestidae","Tetraodontidae","Muraenesocidae",
                            "Cynoglossidae","Sphyraenidae","Lepisosteidae",
                            "Elasmobranchii","Coryphaenidae","Elopidae","Belonidae"))

sealifebase_sp <- species_list |>
  dplyr::filter(class %in% c("Insecta","Malacostraca","Mollusca","Polychaeta",
                            "Invertebrate","Cephalopoda","Bivalvia","Crustacea"))

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
species_list[which(species_list$scientific_name %in% c("Zoobenthos","Macrozoobenthos","Meiofauna","Heterotrophic benthos","Epibenthos","Meiobenthos")), c("class","family")] <- c("Benthos","Benthos")
species_list[which(species_list$scientific_name == "Microcrustacean"), c("class")] <- c("Crustacea")
species_list[which(species_list$scientific_name %in% c("Benthic producer","Phytobenthos","Shrubs","Mosses","Forbs","Grasses","Macrophytes","Lichens")), c("class","family")] <- c("Producer","Producer")
species_list[which(species_list$scientific_name %in% c("Benthic deposit feeders","Benthic suspension feeders","Infauna")), c("class","family")] <- c("Invertebrate","Invertebrate")
species_list[which(species_list$scientific_name == "Shorebirds"), c("class","family")] <- c("Aves","Scolopacidae")
species_list[which(species_list$scientific_name == "Passerines"), c("class")] <- c("Aves")
species_list[which(species_list$scientific_name == "Waterfowl"), c("class","family")] <- c("Aves","Anatidae")
species_list[which(species_list$scientific_name == "Geese"), c("class","family")] <- c("Aves","Anatidae")
species_list[which(species_list$scientific_name == "Gulls"), c("class","family")] <- c("Aves","Laridae")
species_list[which(species_list$scientific_name == "Ptarmigan"), c("class","family")] <- c("Aves","Phasianidae")
species_list[which(species_list$scientific_name == "Seabirds"), c("class")] <- c("Aves")
species_list[which(species_list$scientific_name %in% c("Planktivorous haplochromines","Predatory haplochromines","Benthivorous haplochromines","Cichlids")), c("class","family")] <- c("Actinopterygii","Cichlidae")
species_list[which(species_list$scientific_name == "Lemmings"), c("class","family")] <- c("Mammalia","Cricetidae")
species_list[which(species_list$scientific_name %in% c("Croaker","Large scianidae")), c("class","family")] <- c("Actinopterygii","Sciaenidae")
species_list[which(species_list$scientific_name == "Shrimps"), c("class","family")] <- c("Malacostraca","Penaeidae")
species_list[which(species_list$scientific_name == "Snook"), c("class","family")] <- c("Actinopterygii","Centropomidae")
species_list[which(species_list$scientific_name == "Crabs"), c("class","family")] <- c("Malacostraca","Portunidae")
species_list[which(species_list$scientific_name == "Snout"), c("class")] <- c("Actinopterygii")
species_list[which(species_list$scientific_name == "Arthropoda"), c("class")] <- c("Arthropoda")
species_list[which(species_list$scientific_name == "Sheephead"), c("class","family")] <- c("Actinopterygii","Sparidae")
species_list[which(species_list$scientific_name == "Small cephalopod"), c("class","family")] <- c("Cephalopoda","Loliginidae")
species_list[which(species_list$scientific_name == "Synodus sp"), c("class","family")] <- c("Actinopterygii","Synodontidae")
species_list[which(species_list$scientific_name == "Rays Sharks"), c("class")] <- c("Chondrichthyes")
species_list[which(species_list$scientific_name == "Snapper Grunts"), c("class")] <- c("Actinopterygii")
species_list[which(species_list$scientific_name == "Crustacea and Molluscs"), c("class")] <- c("Crustacea")
species_list[which(species_list$scientific_name == "Mangroves"), c("class")] <- c("Plantae")
species_list[which(species_list$scientific_name == "Demersal shark"), c("class")] <- c("Chondrichthyes")
species_list[which(species_list$scientific_name == "Oreochromis"), c("class","family")] <- c("Actinopterygii","Cichlidae")
species_list[which(species_list$scientific_name == "Grapsid"), c("class")] <- c("Malacostraca")
species_list[which(species_list$scientific_name == "Fishing birds"), c("class")] <- c("Aves")
species_list[which(species_list$scientific_name == "Pleuronectiforms"), c("class")] <- c("Actinopterygii")

################################
## Manual entry of bodymasses ##
################################
# For every fish where possible, the data were taken from Fishbase with the Bayesian L-W,
# (the Bayesian L-W relationships can be found at the bottom of the Fishbase page for a species)

# bodymass_min are computed using the smallest length found with the lower bounds for parameters a and b
# bodymass_max are computed using the largest length found with the highest bounds for parameters a and b
# bodymass_mean are computed using common length if available, if not common length not avaivable and not
# found elsewhere (with the source) I used max-(max-min/2) and using the average parameters a and b values

# For some species, bodymass_max is the max reported in litterature from the fishbase page (will be specified if the case)
# Proceeded this way, because some of the bodymass_max didn't really make sense

# If we only have length_max (quite some cases), we use length_max to compute bodymass_min and bodymass_mean
# but we take the a_min with b_min and a_mean and b_mean in the length-weight relationships

# For some species with way less information, sometime the same length is used for length min,mean and max
# but to compute the bodymass min,mean and max the respective lower-bound, mean and highest-bound of parameters   # a and b are used.

# Bagrus docmak (https://fishbase.in/summary/Bagrus-docmak.html)
species_list[which(species_list$scientific_name == "Bagrus docmak"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20,62,127,0.00789,0.010000,0.01267,2.90,2.96,3.02)

# Carcharhinus melanopterus (https://fishbase.in/summary/Carcharhinus-melanopterus.html)
species_list[which(species_list$scientific_name == "Carcharhinus melanopterus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(91,120,200,0.00252,0.00468,0.00868,2.88,3.03,3.18)

# Chaetodipterus faber (https://fishbase.in/summary/Chaetodipterus-faber.html) max weight reported
species_list[which(species_list$scientific_name == "Chaetodipterus faber"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max","bodymass_max")] <-
                                   c(12,50,91,0.02599,0.03388,0.04418,2.86,2.94,3.02,9000)

# Clarias gariepinus (https://fishbase.in/summary/Clarias-gariepinus.html)
species_list[which(species_list$scientific_name == "Clarias gariepinus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(30.8,90,170,0.00671,0.00813,0.00985,2.93,2.98,3.03)

# Engraulicypris sardella (https://fishbase.in/summary/Engraulicypris-sardella.html)
species_list[which(species_list$scientific_name == "Engraulicypris sardella"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(NA,10,13,0.00254,0.00447,0.00785,2.85,3.01,3.17)
  
# Haplochromis angustifrons (https://fishbase.in/summary/Haplochromis-angustifrons.html)
species_list[which(species_list$scientific_name == "Haplochromis angustifrons"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(9,9,9,0.00244,0.01000,0.04107,2.81,3.04,3.27)

# Haplochromis nigripinnis (https://fishbase.in/summary/Haplochromis-nigripinnis.html)
species_list[which(species_list$scientific_name == "Haplochromis nigripinnis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(6.8,6.8,6.8,0.00708,0.01445,0.02949,2.80,2.97,3.14)

# Haplochromis squamipinnis (https://fishbase.in/summary/Haplochromis-squamipinnis.html)
species_list[which(species_list$scientific_name == "Haplochromis squamipinnis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20.2,20.2,20.2,0.00687,0.01479,0.03186,2.79,2.97,3.15)

# Hydrocynus forskahlii (https://fishbase.in/summary/Hydrocynus-forskahlii.html)
species_list[which(species_list$scientific_name == "Hydrocynus forskahlii"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20,49,78,0.00384,0.00759,0.01500,2.97,3.15,3.33)

# Hydrocynus vittatus (https://fishbase.in/summary/Hydrocynus-vittatus.html)
species_list[which(species_list$scientific_name == "Hydrocynus vittatus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(39.8,74,105,0.00870,0.01096,0.01382,2.95,3.01,3.07)

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
                                   
# Mollusca -> arbitrary
species_list[which(species_list$scientific_name == "Mollusca"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,10.25,20)

# Microcrustacean -> arbitrary
species_list[which(species_list$scientific_name == "Microcrustacean"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,12.75, 25)
  
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

# Zoobenthos -> Chaoborus spp., Copepods, Oligochaetes (arbitrary)
species_list[which(species_list$scientific_name == "Zoobenthos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.05,0.525,1)

# Phytobenthos -> Macroalgae, Chlorophytes (green algae) and Pheophytes (brown algae), Rhodophytes (red algae) -> arbitrary
species_list[which(species_list$scientific_name == "Phytobenthos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.005,0.5025,1) 

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

# Shrimps:
# min based on Eusergestes arcticus (https://sealifebase.ca/summary/Eusergestes-arcticus.html)
# mean based on Pasiphaea multidentata (https://sealifebase.ca/summary/Pasiphaea-multidentata.html)
# max based on Penaeus setiferus (https://sealifebase.ca/summary/Penaeus-setiferus.html)
species_list[which(species_list$scientific_name == "Shrimps"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(0.5,10.5,19.9,0.2858,0.0037,0.0036,2.956,3.234,3.283)

# Anchoa
# min based on Anchoa mitchilli (https://fishbase.in/summary/Anchoa-mitchilli.html)
# mean based on Anchoa lucida (https://www.fishbase.se/summary/Anchoa-lucida.html)
# max based on Anchoa macrolepidota (https://www.fishbase.se/summary/Anchovia-macrolepidota.html)
species_list[which(species_list$scientific_name == "Anchoa"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(4.3,10,25,0.00420,0.00646,0.00915,3.12,3.03,3.36)

# Croaker
# Min based on Bairdiella chrysoura (https://www.fishbase.se/summary/Bairdiella-chrysoura.html)
# Mean based on Bairdiella ronchus (https://www.fishbase.se/summary/Bairdiella-ronchus.html)
# Max based on Micropogonias undulatus (https://www.fishbase.se/summary/Micropogonias-undulatus.html)
species_list[which(species_list$scientific_name == "Croaker"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(9.3,25,55,0.00768,0.00871,0.01283,2.96,3.11,3.30)

# Gerreidae
# Min based on Diapterus rhombeus (https://www.fishbase.se/summary/Diapterus-rhombeus.html)
# Mean based on Eucinostomus melanopterus (https://www.fishbase.se/summary/Eucinostomus-melanopterus.html)
# Max based on Diapterus auratus (https://www.fishbase.se/summary/Diapterus-auratus.html)
species_list[which(species_list$scientific_name == "Gerreidae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(13,23,42.8,0.01020,0.01047,0.01339,2.98,3.08,3.13)

# Crustacea -> Clivanarius vittatus, decapod, amphipod
# Used 10.1017/S0025315409991408 for lowest (amphipoda) and the lowest of decpoda
# bodymass_mean is midpoint between both
species_list[which(species_list$scientific_name == "Crustacea"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.02,7.980448,15.94638)

# Gobiidae: Bathygobius soporator, Gobionellus boleosoma, Aboma etheostoma, Ctenogobius sagittula, Gobionellus microdon
# Min based on Aboma etheostoma (https://www.fishbase.se/summary/Aboma-etheostoma.html)
# Mean based on Bathygobius soporator but(https://www.fishbase.se/summary/Bathygobius-soporator.html)
# Max based on Ctenogobius sagittula (https://www.fishbase.se/summary/Ctenogobius-sagittula.html)
species_list[which(species_list$scientific_name == "Gobiidae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(3.4,15,20,0.00465,0.00759,0.01104,2.85,3.10,2.98)

# Scombridae
# Min based on Rastrelliger kanagurta (https://www.fishbase.se/summary/Rastrelliger-kanagurta.html)
# Mean and max from Euthynnus affinis (https://www.fishbase.se/summary/Euthynnus-affinis.html)
species_list[which(species_list$scientific_name == "Scombridae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(25,60,100,0.00787,0.01000,0.01164,3.02,3.05,3.09)

# Small pelagic fish:
# Min based on Neopisthopterus tropicus (common length)(https://www.fishbase.se/summary/Neoopisthopterus-tropicus.html)
# Mean based on Pellona ditchela (https://www.fishbase.se/summary/Pellona-ditchela.html)
# Max based on Hilsa kelee (https://www.fishbase.se/summary/Hilsa-kelee.html)
species_list[which(species_list$scientific_name == "Small pelagic fish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(6,13,35,0.00741,0.00692,0.01629,3.14,3.10,3.15)

# Demersal fish
# Min from Johnius dussumieri
# Mean from Pomadasys maculatus
# Max from Otolithes ruber
species_list[which(species_list$scientific_name == "Demersal fish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(11,30,90,0.00646,0.01738,0.00935,3.05,2.93,3.10)

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

# Crabs:
# Used the largest species(C. sapidus) (https://sealifebase.ca/summary/Callinectes-sapidus.html)
# Used the one that ranges from 6.0-21.3
species_list[which(species_list$scientific_name == "Crabs"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(6,13.65,21.3,0.1182,0.1182,0.1182,2.772,2.772,2.772)

# Meiofauna -> arbitrary
species_list[which(species_list$scientific_name == "Meiofauna"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.005,0.2525,0.5)

# Snout -> Synodontis and mormyrus
# Used Synodontis victoriae (https://fishbase.in/summary/Synodontis-victoriae.html)
# lower-bound from L-W relationship, higher from article
species_list[which(species_list$scientific_name == "Snout"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max","bodymass_max")] <-
                                   c(8,21.5,NA,0.00631,0.01230,NA,2.77,2.94,NA,650)

# Snook:
# Min from Centropomus robalito, used common length (https://www.fishbase.se/summary/Centropomus-robalito.html)
# Mean from Centropomus nigrescens, used common length (https://www.fishbase.se/summary/Centropomus-nigrescens.html)
# Max from Centropomus undecimalis (https://www.fishbase.se/summary/Centropomus-undecimalis.html)
species_list[which(species_list$scientific_name == "Snook"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(25,45,140,0.00741,0.00724,0.00861,3.06,3.02,3.09)

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

# Chaoborus edulis
# Used the same as Zoobenthos, since Chaoborus is part of this group
species_list[which(species_list$scientific_name == "Chaoborus edulis"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.05,0.525,1)

# Large catfish
# Based of Bagrus docmak (https://fishbase.in/summary/Bagrus-docmak.html)
species_list[which(species_list$scientific_name == "Large catfish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20,62,127,0.00730,0.00933,0.01193,2.94,3.00,3.06)

# Small catfish
# Min based on Ariopsis felis (https://www.fishbase.se/summary/Ariopsis-felis.html)
# Mean based on Notarius kessleri (https://www.fishbase.se/summary/Notarius-kessleri.html)
# Max based on Sciades dowii (https://www.fishbase.se/summary/Arius-dovii.html)
species_list[which(species_list$scientific_name == "Small catfish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(13,40,90,0.00491,0.00479,0.00657,3.07,3.13,3.32)

# Heterotrophic benthos -> same as Macrozoobenthos
species_list[which(species_list$scientific_name == "Heterotrophic benthos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,1.25,2)

# Crustacea and Molluscs
# Used Scylla serrata (https://sealifebase.ca/summary/Scylla-serrata.html)
species_list[which(species_list$scientific_name == "Crustacea and Molluscs"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(1,12,20.6,0.4580,0.4580,0.4580,2.653,2.653,2.653)

# Lichens
species_list[which(species_list$scientific_name == "Lichens"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,0.75,1)

# Engraulicypris sardella (https://www.fishbase.de/summary/Engraulicypris-sardella.html)
# Arbitrary length_min
species_list[which(species_list$scientific_name == "Engraulicypris sardella"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(7,10,13,0.00254,0.00447,0.00785,2.85,3.01,3.17)

# Stercorarius longicaudus
# (https://en.wikipedia.org/wiki/Long-tailed_jaeger)
species_list[which(species_list$scientific_name == "Stercorarius longicaudus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(230,337,444)

# Arthropoda -> Chironomidae, Muscidae, Aranea, Lepidoptera, Ichneumonidae, Carabidae
# Arbitrary
species_list[which(species_list$scientific_name == "Arthropoda"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,0.75,1)

# Alepisaurus -> A. minutus and A. ferox
# Based on Alepisaurus ferox (https://www.fishbase.de/summary/Alepisaurus-ferox.html)
species_list[which(species_list$scientific_name == "Alepisaurus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(46,150,215,0.00180,0.00389,0.00842,2.94,3.12,3.30)

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

# Copadichromis azureus -> Bayesian L-W from fishbase (https://fishbase.in/summary/Copadichromis-azureus.html)
species_list[which(species_list$scientific_name == "Copadichromis azureus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(14.6,14.6,14.6,0.00687,0.01479,0.03186,2.79,2.97,3.15)

# Bubo scandiacus (https://en.wikipedia.org/wiki/Snowy_owl)
species_list[which(species_list$scientific_name == "Bubo scandiacus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(1300,1872.5,2951)

# Buteo lagopus (https://en.wikipedia.org/wiki/Rough-legged_buzzard)
species_list[which(species_list$scientific_name == "Buteo lagopus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(600,1051.75,1660)

#  Canis lupus arctos (https://fr.wikipedia.org/wiki/Loup_arctique)
# bodymass_min -> (https://www.museedelhistoire.ca/cmc/exhibitions/hist/cae/col1111bf.html)
species_list[which(species_list$scientific_name == "Canis lupus arctos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(19000, 45000, 80000)

# Carangidae
# Min from Hemicaranx zelotes, used common length (https://www.fishbase.se/summary/Hemicaranx-zelotes.html)
# Mean from Caranx sexfasciatus (https://www.fishbase.se/summary/Caranx-sexfasciatus.html)
# Max from Caranx hippos (https://www.fishbase.se/summary/Caranx-hippos.html)
species_list[which(species_list$scientific_name == "Carangidae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20,60,124,0.01096,0.01820,0.02082,2.94,2.95,2.98)

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

# Rhamphochromis longiceps
# Bayesian L-W from fishbase (https://www.fishbase.ca/summary/Rhamphochromis-longiceps.html)
species_list[which(species_list$scientific_name == "Rhamphochromis longiceps"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(13,20.5,28,0.00244,0.01000,0.04107,2.81,3.04,3.27)

# Seabirds
# Calidris alpina,Haemotopus ostrealegus,Numenius aquatus,Fulica atra,Anas crecca,Anas plathyrhynchus,Larus ridibundus,Larus argentus, Podiceps cristatus, Phalocrocorax carbo
# bodymass_min from Tachybaptus dominicus (https://en.wikipedia.org/wiki/Least_grebe)
# bodymass_mean is midpoint
# bodymass_max from Pelicanus sp.(https://en.wikipedia.org/wiki/Pelican)
species_list[which(species_list$scientific_name == "Seabirds"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(180,5090,10000)

# Fishing birds
# bodymass_min from kingfishers(https://en.wikipedia.org/wiki/Belted_kingfisher)
# bodymass_mean is midpoint
# bodymass_max from Pelicanus sp.(https://en.wikipedia.org/wiki/Pelican)
species_list[which(species_list$scientific_name == "Fishing birds"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(113,5065.5,10000)

# Sheephead
# bodymass_min from Lagodon rhomboides (https://www.fishbase.de/summary/Lagodon-rhomboides.html)
#bodymass_mean and max from Archosargus probatocephalus (https://www.fishbase.de/summary/Archosargus-probatocephalus.html)
species_list[which(species_list$scientific_name == "Sheephead"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(9,35,91,0.01189,0.02188,0.02622,3.01,3.04,3.07)

# Stercorarius parasiticus (https://en.wikipedia.org/wiki/Parasitic_jaeger)
species_list[which(species_list$scientific_name == "Stercorarius parasiticus"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(300,475,650)

# Ursus arctos (https://en.wikipedia.org/wiki/Brown_bear)
species_list[which(species_list$scientific_name == "Ursus arctos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(135000,166000,217000)

# Vulpes vulpes (https://fr.wikipedia.org/wiki/Renard_roux)
species_list[which(species_list$scientific_name == "Vulpes vulpes"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(2200,8100,14000)

# Zooplankton gelatinous
# Arbitrary
species_list[which(species_list$scientific_name == "Zooplankton gelatinous"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.05,1.025,2)

#Aristeus antennatus (https://sealifebase.ca/summary/Aristeus-antennatus.html)
# 1.2 - 6.2
species_list[which(species_list$scientific_name == "Aristeus antennatus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(1.2,3.7,6.2,1.2974,1.2974,1.2974,2.055,2.055,2.055)

# Penaeus stylirostris (https://sealifebase.ca/summary/Penaeus-stylirostris.html)
species_list[which(species_list$scientific_name == "Litopenaeus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(1.9,9.05,16.2,0.0061,0.0061,0.0061,3.072,3.072,3.072)

# Infauna, same as Zoobenthos
species_list[which(species_list$scientific_name == "Infauna"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(c(0.05,0.525,1))

# Polychaeta, same as Nereidae
species_list[which(species_list$scientific_name == "Polychaeta"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(c(0.007,0.3235,0.64))

# Thryssa setirostris (https://www.fishbase.se/summary/Thryssa-setirostris.html)
species_list[which(species_list$scientific_name == "Thryssa setirostris"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(12,15,18,0.00437,0.00724,0.01201,3.01,3.15,3.29)

# Encrasicholina heteroloba (https://www.fishbase.se/summary/Encrasicholina-heteroloba.html)
species_list[which(species_list$scientific_name == "Encrasicholina heteroloba"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(5,7.5,12,0.00480,0.00562,0.00659,3.11,3.15,3.19)

# Amblygaster sirm (https://www.fishbase.se/summary/Amblygaster-sirm.html)
species_list[which(species_list$scientific_name == "Amblygaster sirm"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(15,20,27,0.00722,0.00832,0.00958,3.00,3.04,3.08)

# Brachirus orientalis (https://www.fishbase.se/summary/Brachirus-orientalis.html)
# Used range  10.2 - 38.0 
species_list[which(species_list$scientific_name == "Brachirus orientalis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(10.2,24.1,38,0.019,0.019,0.019,2.927,2.927,2.927)

# Lepturacanthus savala (https://www.fishbase.se/summary/Lepturacanthus-savala.html)                          
species_list[which(species_list$scientific_name == "Lepturacanthus savala"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(38,70,100,0.00046,0.00066,0.00096,2.92,3.03,3.14)

# Carangoides malabaricus (https://www.fishbase.se/summary/Carangoides-malabaricus.html)
# Used range 15.0 - 44.0 
species_list[which(species_list$scientific_name == "Carangoides malabaricus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(15,29.5,44,0.0233,0.0233,0.0233,3.02,3.02,3.02)

# Chanos chanos (https://www.fishbase.se/summary/Chanos-chanos.html)
species_list[which(species_list$scientific_name == "Chanos chanos"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(68,100,180,0.00654,0.00851,0.01108,2.98,3.06,3.14)

# Sardinella gibbosa (https://www.fishbase.se/summary/Sardinella-gibbosa.html)    
species_list[which(species_list$scientific_name == "Sardinella gibbosa"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(11,15,29.6,0.00679,0.00832,0.01019,3.00,3.03,3.06)

# Parastromateus niger (https://www.fishbase.se/summary/Parastromateus-niger.html)
species_list[which(species_list$scientific_name == "Parastromateus niger"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(22,30,75,0.02121,0.02570,0.03115,2.85,2.90,2.95)

# Hilsa kelee (https://www.fishbase.se/summary/Hilsa-kelee.html)
species_list[which(species_list$scientific_name == "Hilsa kelee"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(15,16.5,35,0.00643,0.01023,0.01629,2.89,3.02,3.15)

# Dormitator latifrons (https://www.fishbase.se/summary/Dormitator-latifrons.html)
species_list[which(species_list$scientific_name == "Dormitator latifrons"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(17,25,41,0.00779,0.01072,0.01474,3.02,3.11,3.20)

# Omnivore fish
# Min from Astyanax aeneus (https://www.fishbase.se/summary/Astyanax-aeneus.html)
# Mean from Polydactylus approximans (https://www.fishbase.se/summary/Polydactylus-approximans.html)
# Max from Kyphosus elegans (https://www.fishbase.se/summary/Kyphosus-elegans.html)
species_list[which(species_list$scientific_name == "Omnivore fish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(7.5,20,53,0.00766,0.00661,0.04135,3.00,3.06,3.21)

# Eleotridae                         
# From Eleotris picta with range 13.0 - 43.4 
species_list[which(species_list$scientific_name == "Eleotridae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(13,28.2,43.4,0.0084,0.0084,0.0084,3.15,3.15,3.15)

# Macrobrachium, based on Macrobrachium americanum (https://sealifebase.ca/summary/Macrobrachium-americanum.html)
# Used range  1.4 - 9.7
species_list[which(species_list$scientific_name == "Macrobrachium"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(1.4,5.55,9.7,0.1057,0.1057,0.1057,3.48,3.48,3.48)

# Carnivore fish
# Min from Roeboides bouchellei (https://www.fishbase.se/summary/Roeboides-bouchellei.html)
# Mean and Max from Elops affinis (https://www.fishbase.se/summary/Elops-affinis.html)
species_list[which(species_list$scientific_name == "Carnivore fish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(8.2,50,91,0.00448,0.00562,0.01290,2.89,3.02,3.21)

# Poeciliidae, use Poecilia sphenops (https://www.fishbase.se/summary/Poecilia-sphenops.html)
# Range 1.8 - 7.1 
species_list[which(species_list$scientific_name == "Poeciliidae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(1.8,4.45,7.1,0.028,0.028,0.028,2.97,2.97,2.97)

# Katsuwonus pelamis (https://www.fishbase.se/summary/Katsuwonus-pelamis.html)
species_list[which(species_list$scientific_name == "Katsuwonus pelamis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(40,80,110,0.00978,0.01096,0.01229,3.07,3.10,3.13)

# Rastrelliger kanagurta (https://www.fishbase.se/summary/Rastrelliger-kanagurta.html)
species_list[which(species_list$scientific_name == "Rastrelliger kanagurta"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20,25,36,0.00787,0.00891,0.01010,3.02,3.06,3.10)

# Scomberomorus commerson (https://www.fishbase.se/summary/Scomberomorus-commerson.html)
species_list[which(species_list$scientific_name == "Scomberomorus commerson"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(55,120,240,0.00596,0.00676,0.00766,2.97,3.00,3.03)

# Epibenthos
# Same as Zoobenthos
species_list[which(species_list$scientific_name == "Epibenthos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.05,0.525,1)

# Pleuronectiforms
# Min from Etropus crossotus (https://www.fishbase.se/summary/Etropus-crossotus.html)
# Mean from Symphurus chabanaudi (https://www.fishbase.se/summary/Symphurus-chabanaudi.html)
# Max from Cyclopsetta querna (https://www.fishbase.se/summary/Cyclopsetta-querna.html) 
species_list[which(species_list$scientific_name == "Pleuronectiforms"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(8,21,39,0.00660,0.00617,0.01216,3.03,3.09,3.28)

# Hemiramphus
# Used Hemiramphus brasiliensis (https://www.fishbase.se/summary/Hemiramphus-brasiliensis.html)
species_list[which(species_list$scientific_name == "Hemiramphus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(19.3,35,55,0.00268,0.00372,0.00515,3.02,3.08,3.14)

# Small demersal fish
# Assign the same as Gerreida since they are 'small'
# And Gerreidae is a part of this group
species_list[which(species_list$scientific_name == "Small demersal fish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(13,23,42.8,0.01020,0.01047,0.01339,2.98,3.08,3.13)

# Mugilidae
# Min from Mugil hospes (https://www.fishbase.se/summary/Mugil-hospes.html)
# Mean and Max from Mugil cephalus (https://www.fishbase.se/summary/Mugil-cephalus.html)
species_list[which(species_list$scientific_name == "Mugilidae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(15,50,100,0.00623,0.01202,0.01336,2.71,2.94,2.97)                                  

# Small cephalopod
# Based on Lolliguncula panamensis (https://sealifebase.ca/summary/Lolliguncula-panamensis.html)
# Range used  2.5 - 11.5
species_list[which(species_list$scientific_name == "Small cephalopod"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(2.5,7,11.5,0.0778,0.0778,0.0778,2.59,2.59,2.59)

# Clupeidei
# Min from Atherinomorus stipes, common length (https://www.fishbase.se/summary/Atherinomorus-stipes.html)
# Mean from Engraulis mordax, common length (https://www.fishbase.se/summary/Engraulis-mordax.html)
# Max from Dorosoma petenense (https://www.fishbase.se/summary/Dorosoma-petenense.html)
species_list[which(species_list$scientific_name == "Clupeidei"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(7.5,15,33,0.00724,0.00550,0.01458,3.21,3.08,3.12)

# Selar crumenophthalmus (https://www.fishbase.se/summary/Selar-crumenophthalmus.html)
# Used range  15.0 - 32.7 
species_list[which(species_list$scientific_name == "Selar crumenophthalmus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(15,23.85,32.7,0.0067,0.0067,0.0067,3.3,3.3,3.3)

# Medium cephalopod
# Based on Todarodes sagittatus, used range  8.1 - 41.8
species_list[which(species_list$scientific_name == "Medium cephalopod"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(8.1,24.95,41.8,0.0102,0.0102,0.0102,3.31,3.31,3.31)
# Insects
# Same as Arthropoda
species_list[which(species_list$scientific_name == "Insects"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,0.75,1)

# Planktivore fish
# Min from Hippocampus ingens (https://www.fishbase.se/summary/Hippocampus-ingens.html)
# Mean from Hyporhamphus snyderi (https://www.fishbase.se/summary/Hyporhamphus-snyderi.html)
# Max from Membras gilberti (https://www.fishbase.se/summary/Membras-gilberti.html)
species_list[which(species_list$scientific_name == "Planktivore fish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(5.4,10,20,0.00171,0.00245,0.01221,2.78,3.10,3.29)

# Leiognathus brevirostris (https://www.fishbase.se/summary/Leiognathus-brevirostris.html)
species_list[which(species_list$scientific_name == "Leiognathus brevirostris"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(3.8,11,18.2,0.00782,0.01380,0.02438,2.81,2.97,3.13)

# Lactarius lactarius (https://www.fishbase.se/summary/Lactarius-lactarius.html)
species_list[which(species_list$scientific_name == "Lactarius lactarius"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(15,30,40,0.00886,0.01259,0.01789,2.86,2.96,3.06)
# Hirundichthys oxycephalus
# Based it on Hirundichthys marginatus since not enough data for Hirundichthys oxycepha
species_list[which(species_list$scientific_name == "Hirundichthys oxycephalus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(9,15,21,0.00162,0.00427,0.01126,2.88,3.12,3.36)

# Chirocentrus dorab (https://www.fishbase.se/summary/Chirocentrus-dorab.html)
# Based on range 32.5 - 63.0
species_list[which(species_list$scientific_name == "Chirocentrus dorab"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20,60,100,0.00356,0.00513,0.00739,2.86,2.96,3.06)

# Selaroides leptolepis (https://www.fishbase.se/summary/Selaroides-leptolepis.html)
species_list[which(species_list$scientific_name == "Selaroides leptolepis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(9,15,22,0.01181,0.01318,0.01471,2.93,2.96,2.99)

# Ariopsis guatemalensis (https://www.fishbase.se/summary/Ariopsis-guatemalensis.html)
# Used range  22.4 - 54.0 
species_list[which(species_list$scientific_name == "Ariopsis guatemalensis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(22.4,38.2,54,0.0037,0.0037,0.0037,3.3,3.3,3.3)

# Meiobenthos
# Same as Nereidae
species_list[which(species_list$scientific_name == "Meiobenthos"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.007,0.3235,0.64)

# Large scianidae
# Min from Cynoscion phoxocephalus, commong length (https://www.fishbase.se/summary/Cynoscion-phoxocephalus.html)
# Mean and Max from Cynoscion albus (https://www.fishbase.se/summary/Cynoscion-albus.html)
species_list[which(species_list$scientific_name == "Large scianidae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(25,50,130,0.00813,0.00794,0.01259,3.01,3.06,3.19)

# Synodus sp
# Min from Lmaturity of Synodus evermanni (https://www.fishbase.se/summary/Synodus-evermanni.html)
# Mean from Synodus evermanni (https://www.fishbase.se/summary/Synodus-evermanni.html)
# Max from Synodus scituliceps (https://www.fishbase.se/summary/Synodus-scituliceps.html)
species_list[which(species_list$scientific_name == "Synodus sp"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(14,20,51,0.00344,0.00550,0.00635,2.92,3.06,3.22)

# Rays Sharks
# Min from Narcine brasiliensis, used Max length (https://www.fishbase.se/summary/Narcine-brasiliensis.html)
# Mean from Pseudobatos leucorhynchus, used max length (https://www.fishbase.se/summary/Pseudobatos-leucorhynchus.html)
# Max from Mustelus lunulatus (https://www.fishbase.se/summary/Mustelus-lunulatus.html)
species_list[which(species_list$scientific_name == "Rays Sharks"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(54,118,175,0.02082,0.00419,0.00523,3.07,3.24,3.32)

# Snapper Grunts
# Min from Genyatremus dovii, used common length (https://www.fishbase.se/summary/Anisotremus-dovii.html)
# Mean from Lutjanus guttatus, used max length (https://www.fishbase.se/summary/Calamus-brachysomus.html)
# Max from Calamus brachysomus (https://www.fishbase.se/summary/Lutjanus-guttatus.html)
species_list[which(species_list$scientific_name == "Snapper Grunts"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(35,80,61,0.01950,0.02080,0.04735,2.95,3.04,3.12)

# Phycis blennoides (https://www.fishbase.se/summary/Phycis-blennoides.html)
species_list[which(species_list$scientific_name == "Phycis blennoides"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(33,71.5,110,0.00384,0.00468,0.00570,3.07,3.13,3.19)

# Alepocephalus rostratus
species_list[which(species_list$scientific_name == "Alepocephalus rostratus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(27.7,38.85,50,0.00189,0.00316,0.00529,3.02,3.17,3.32)

# Benthopelagic fish
# Min from Argyropelecus hemigymnus (https://www.fishbase.se/summary/Argyropelecus-hemigymnus.html)
# Mean from Argyropelecus hemigymnus, used max length (https://www.fishbase.se/summary/Argyropelecus-hemigymnus.html)
# Max from Lampanyctus crocodilus (https://www.fishbase.se/summary/Lampanyctus-crocodilus.html)
species_list[which(species_list$scientific_name == "Benthopelagic fish"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(3,5.1,30,0.00964,0.03942,0.00582,2.87,3.23,3.23)

# Lepidion lepidion
species_list[which(species_list$scientific_name == "Lepidion lepidion"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(29.5,36.75,44,0.00185,0.00302,0.00494,3.07,3.21,3.35)

# Demersal fish2
# Used Nettastoma melanura (https://www.fishbase.se/summary/Nettastoma-melanura.html)
species_list[which(species_list$scientific_name == "Demersal fish2"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(52,60.9,79.8,0.00066,0.00095,0.00138,2.85,2.95,3.05)

# Mora moro  (https://www.fishbase.se/summary/Mora-moro.html)
species_list[which(species_list$scientific_name == "Mora moro"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(10,45,80,0.00481,0.00646,0.00867,3.05,3.13,3.21)

# Lophius piscatorius (https://www.fishbase.se/summary/Lophius-piscatorius.html)
species_list[which(species_list$scientific_name == "Lophius piscatorius"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(35,100,200,0.01360,0.01738,0.02220,2.83,2.90,2.97)

# Macrouridae
# Min and Mean (is the max) from Nezumia aequalis (https://www.fishbase.se/summary/Nezumia-aequalis.html)
# Max from Trachyrincus scabrus (https://www.fishbase.se/summary/Trachyrincus-scabrus.html)
species_list[which(species_list$scientific_name == "Macrouridae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(4,36,60,0.00112,0.00310,0.00801,2.96,3.26,3.34)

# Haemulidae
# Used Haemulon sexfasciatum (https://www.fishbase.se/summary/Haemulon-sexfasciatum.html)
# Used range  11.0 - 44.0 
species_list[which(species_list$scientific_name == "Haemulidae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(11,27.5,44,0.081,0.081,0.081,2.71,2.71,2.71)

# Lutjanidae
# Min from Lutjanus argentiventris (https://www.fishbase.se/summary/Lutjanus-argentiventris.html)
# Mean from Lutjanus colorado, used max length (https://www.fishbase.se/summary/Lutjanus-colorado.html)
# Max from Lutjanus novemfasciatus (https://www.fishbase.se/summary/Lutjanus-novemfasciatus.html)
species_list[which(species_list$scientific_name == "Lutjanidae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20,91,170,0.01293,0.01853,0.01698,2.94,3.08,3.00)

# Mangroves
# Same as grass
species_list[which(species_list$scientific_name == "Mangroves"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(0.5,0.75,1)

# Demersal shark
# Min from Etmopterus spinax (https://www.fishbase.se/summary/Etmopterus-spinax.html)
# Mean from Galeus melastomus, used max length (https://www.fishbase.se/summary/Galeus-melastomus.html)
# Max from Dalatias licha (https://www.fishbase.se/summary/Dalatias-licha.html)
species_list[which(species_list$scientific_name == "Demersal shark"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(34.5,75,182,0.00279,0.00332,0.00315,3.01,3.07,3.26)

# Terapon theraps (https://www.fishbase.se/summary/Terapon-theraps.html)
# Used range  4.6 - 18.4 
species_list[which(species_list$scientific_name == "Terapon theraps"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(4.6,11.5,18.4,0.013,0.013,0.013,3.019,3.019,3.019)

# Otolithes ruber (https://www.fishbase.se/summary/Otolithes-ruber.html)
species_list[which(species_list$scientific_name == "Otolithes ruber"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(22.6,40,90,0.00775,0.00851,0.00935,3.04,3.07,3.10)

# Sphoeroides annulatus
# Used range  8.6 - 33.0 
species_list[which(species_list$scientific_name == "Sphoeroides annulatus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(8.6,20.8,33,0.018,0.018,0.018,3.3,3.3,3.3)

# Grapsid: Clibanarius panamensis, Sesarma rhizophorae
# Used same as crabs since lacking information on target species
species_list[which(species_list$scientific_name == "Grapsid"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(6,13.65,21.3,0.1182,0.1182,0.1182,2.772,2.772,2.772)

# Cynoponctius coniceps (https://www.fishbase.se/summary/Cynoponticus-coniceps.html)
# Used range  11.2 - 74.5 
species_list[which(species_list$scientific_name == "Cynoponctius coniceps"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(11.2,42.85,74.5,0.004,0.004,0.004,2.686,2.686,2.686)

# Cynoglossus zanzibarensis (https://www.fishbase.se/summary/Cynoglossus-zanzibarensis.html)
# Min from same genus Length-maturity of Cynoglossus lingua
species_list[which(species_list$scientific_name == "Cynoglossus zanzibarensis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(9.4,16,32,0.00447,0.01096,0.02692,2.82,3.03,3.24)

# Rhynchorhamphus malabaricus (https://www.fishbase.se/summary/Rhynchorhamphus-malabaricus.html)
species_list[which(species_list$scientific_name == "Rhynchorhamphus malabaricus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(5,20,35,0.00143,0.00309,0.00666,2.91,3.09,3.27)

# Sphyraena jello (https://www.fishbase.se/summary/Sphyraena-jello.html)
# Used range 21.6 - 93.2
species_list[which(species_list$scientific_name == "Sphyraena jello"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(21.6,57.4,93.2,0.02,0.02,0.02,2.706,2.706,2.706)

# Sciaenidae
# Min from Bairdiella armata (https://www.fishbase.se/summary/Bairdiella-armata.html)
# Mean and Max from Cynoscion albus (https://www.fishbase.se/summary/Cynoscion-albus.html)
species_list[which(species_list$scientific_name == "Sciaenidae"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(20,50,130,0.00415,0.00794,0.01259,3.08,3.06,3.19)

# Atractosteus tropicus (https://www.fishbase.se/summary/Atractosteus-tropicus.html)
species_list[which(species_list$scientific_name == "Atractosteus tropicus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(45.5,85.25,125,0.00119,0.00295,0.00734,2.96,3.18,3.40)

# Crocodilians
# Min from Caiman crocodilus (https://en.wikipedia.org/wiki/Spectacled_caiman)
# Mean and Max from Crocodylus acutus (https://animaldiversity.org/accounts/Crocodylus_acutus/)
species_list[which(species_list$scientific_name == "Crocodilians"), c("bodymass_min","bodymass_mean","bodymass_max")] <- c(7000,220000,400000)

# Mugil cephalus (https://www.fishbase.se/summary/Mugil-cephalus.html)
species_list[which(species_list$scientific_name == "Mugil cephalus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(35.4,50,100,0.01082,0.01202,0.01336,2.91,2.94,2.97)

# Netuma bilineata
# Min from same genus length-maturity Netuma thalassina
# Mean is midpoint
# Max from Netuma bilineata (https://www.fishbase.se/summary/Netuma-bilineata.html)
species_list[which(species_list$scientific_name == "Netuma bilineata"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(33,61.5,90,0.00377,0.00794,0.01673,2.90,3.07,3.24)

# Auxis thazard (https://www.fishbase.se/summary/Auxis-thazard.html)
# Used range 26.0 - 61.1 
species_list[which(species_list$scientific_name == "Auxis thazard"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(26,43.55,61.6,0.02936,0.02936,0.02936,2.871,2.871,2.871)

# Sphyraena obtusata
species_list[which(species_list$scientific_name == "Sphyraena obtusata"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(12,31,50,0.0095,0.0095,0.0095,2.868,2.868,2.868)

# Coryphaena hippurus (https://www.fishbase.se/summary/Coryphaena-hippurus.html)
species_list[which(species_list$scientific_name == "Coryphaena hippurus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(55.8,100,210,0.01051,0.01349,0.01732,2.79,2.87,2.95)

# Elops affinis (https://www.fishbase.se/summary/Elops-affinis.html)
species_list[which(species_list$scientific_name == "Elops affinis"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(9,50,91,0.00245,0.00562,0.01290,2.83,3.02,3.21)

# Hexanchus griseus (https://www.fishbase.se/summary/Hexanchus-griseus.html)
# Used range 250.0 - 600.0
species_list[which(species_list$scientific_name == "Hexanchus griseus"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(250,425,600,0.0062,0.0062,0.0062,3.04,3.04,3.04)

# Strongylura leiura (https://www.fishbase.se/summary/Strongylura-leiura.html)
# Used range 43.5 - 75.0 
species_list[which(species_list$scientific_name == "Strongylura leiura"),
                                   c("length_min","length_mean","length_max","a_min","a_mean","a_max","b_min","b_mean","b_max")] <-
                                   c(43.5,59.25,75,0.00981,0.00981,0.00981,2.515,2.515,2.515)

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
species_list[which(is.na(species_list$bodymass_min & species_list$bodymass_max & species_list$bodymass_mean)),]

# Add species metabolic classes
species_list[which(species_list$class %in% c("Aves", "Mammalia")), "metabolic_class"] <- "endotherm"
species_list[which(species_list$scientific_name == "Fishing birds"), "metabolic_class"] <- "endotherm"
species_list[which(species_list$family %in% c("Istiophoridae","Xiphiidae","Scombridae")), "metabolic_class"] <- "homeotherm"
species_list[which(is.na(species_list$metabolic_class)),"metabolic_class"] <- "ectotherm"

# Save the file
saveRDS(species_list, "data/intermediate/species_traits.RDS")
