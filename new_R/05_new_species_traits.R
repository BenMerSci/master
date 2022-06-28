# Load the interaction table to make a unique species list
resolved_inter_table <- readRDS("data/intermediate/new/resolved_inter_table.RDS")


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
                dplyr::mutate(bodymass_min = NA, bodymass_max = NA)

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
fish <- species_list[which(species_list$class %in%
         c("Actinopterygii", "Elasmobranchii", "Sarcopterygii")), ]
validated_fish <- data.frame(cbind(fish$scientific_name,
                   rfishbase::validate_names(fish$scientific_name))) |>
                  dplyr::rename(original = "X1", validated = "X2")
# Get the length-weight relationships
fish_LW <- rfishbase::length_weight(validated_fish$validated)
fish_LW_ls <- base::split(fish_LW, f = fish_LW$Species)
fish_LW_ls <- purrr::map(fish_LW_ls, ~ dplyr::mutate(., LengthMin = as.numeric(.$LengthMin),
                                              LengthMax = as.numeric(.$LengthMax)) |>
                                       dplyr::mutate(bodymass_min = .$a * .$LengthMin^.$b,
                                              bodymass_max = .$a * .$LengthMax^.$b) |>
                                       dplyr::select(c("Species","bodymass_min","bodymass_max")) |>
                                       dplyr::group_by(Species) |>
                                       dplyr::summarise(bodymass_min = mean(bodymass_min, na.rm = TRUE),
                                        bodymass_max = mean(bodymass_max, na.rm = TRUE))                    
                        )

fish_LW_ls <- do.call("rbind", fish_LW_ls)
species_list$bodymass_min <- fish_LW_ls$bodymass_min[match(species_list$scientific_name,fish_LW_ls$Species)]
species_list$bodymass_max <- fish_LW_ls$bodymass_max[match(species_list$scientific_name,fish_LW_ls$Species)]

# Check which species are NAs for bodymass
missing_mass <- species_list[which(is.na(species_list$bodymass_min & species_list$bodymass_max)),]

################################
## Manual entry of bodymasses ##
################################

# Other fish1
species_list[which(species_list$scientific_name == "Other fish1"), c("bodymass_min","bodymass_max")] <- rfishbase::length_weight(c("Trisopterus luscus","Pleuronectes platessa","Clupea harengus","Sprattus sprattus","Solea solea","Pomatoschistus microps"), server = "fishbase") |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Mollusca
species_list[which(species_list$scientific_name == "Mollusca"), c("bodymass_min","bodymass_max")] <- c(0.5,50)

# Microcrustacean
species_list[which(species_list$scientific_name == "Microcrustacean"), c("bodymass_min","bodymass_max")] <- c(0.5, 25)

# Decapoda -> Penaeidae & brachyura
# Mixed the shrimps and crabs mass.
shrimps <- c(0.7133*(1.2^2.940), 0.0084*(10.8^2.956)) # shrimps
crabs <- rfishbase::length_weight(c("Callinectes danae", "Callinectes boucorti", "Callinectes sapidus"), server = "sealifebase") |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)
decapoda <- rbind(shrimps,crabs) |>
            colMeans()
species_list[which(species_list$scientific_name == "Decapoda"), c("bodymass_min","bodymass_max")] <- decapoda
            
# Nereidae, based on (doi:10.1017/S0025315409991408)
species_list[which(species_list$scientific_name == "Nereidae"), c("bodymass_min","bodymass_max")] <- c(0.01,3.31)

# Benthic producer -> arbitrary decision (only small benthic prod, seagrass, macrophytes)
species_list[which(species_list$scientific_name == "Benthic producer"), c("bodymass_min","bodymass_max")] <- c(0.05,1) 

# Zooplankton
species_list[which(species_list$scientific_name == "Zooplankton"), c("bodymass_min","bodymass_max")] <- c(0.0001, 1) # Yodzis

# Phytoplankton
species_list[which(species_list$scientific_name == "Phytoplankton"), c("bodymass_min","bodymass_max")] <- c(0.0001, 0.001) # Yodzis

# Forbs -> arbitrary
species_list[which(species_list$scientific_name == "Forbs"), c("bodymass_min","bodymass_max")] <- c(0.5,1)

# Shrubs -> arbitrary
species_list[which(species_list$scientific_name == "Shrubs"), c("bodymass_min","bodymass_max")] <- c(0.5,1)

# Mosses -> arbitrary
species_list[which(species_list$scientific_name == "Mosses"), c("bodymass_min","bodymass_max")] <- c(0.5,1)

# Grasses -> arbitrary
species_list[which(species_list$scientific_name == "Grasses"), c("bodymass_min","bodymass_max")] <- c(0.5,1)

# Eucinostomus
species_list[which(species_list$scientific_name == "Eucinostomus"), c("bodymass_min","bodymass_max")] <- rfishbase::length_weight(c("Eucinostomus gula", "Eucinostomus argenteus")) |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Haplochromis nigripinnis, fishbase bayesian L-W relationships
species_list[which(species_list$scientific_name == "Haplochromis nigripinnis"), c("bodymass_min","bodymass_max")] <- c(0.00708*(6.8^2.80),0.02949*(6.8^3.14))

# Zoobenthos -> Chaoborus spp., Copepods, Oligochaetes (arbitrary)
species_list[which(species_list$scientific_name == "Zoobenthos"), c("bodymass_min","bodymass_max")] <- c(0.05,1)

# Haplochromis angustifrons, fishbase bayesian L-W relationships
species_list[which(species_list$scientific_name == "Haplochromis angustifrons"), c("bodymass_min","bodymass_max")] <- c(0.00244*(9^2.81),0.04107*(9^3.27))

# Haplochromis squamipinnis, fishbase bayesian L-W relationships
species_list[which(species_list$scientific_name == "Haplochromis squamipinnis"), c("bodymass_min","bodymass_max")] <- c(0.00687*(20.2^2.79),0.03186*(20.2^3.15))

# Phytobenthos -> Macroalgae, Chlorophytes (green algae) and Pheophytes (brown algae), Rhodophytes (red algae) -> arbitrary
species_list[which(species_list$scientific_name == "Phytobenthos"), c("bodymass_min","bodymass_max")] <- c(0.005,1) 

# Benthic deposit feeders (Pectinaria koreni, Macoma balthica and Owenia fusiformis)
# Used 10.1017/S0025315409991408, for the only Terebellida present (for Pectinaria koreni)
species_list[which(species_list$scientific_name == "Benthic deposit feeders"), c("bodymass_min","bodymass_max")] <- c(0.05,10) 

# Benthic suspension feeders (Cerastoderma edule and Abra alba), mean of both with L-W relationships on sealifebase
species_list[which(species_list$scientific_name == "Benthic suspension feeders"), c("bodymass_min","bodymass_max")] <- c(0.1205*(2.3^2.835),1.1570*(5.4^2.260))

# Caridina nilotica
species_list[which(species_list$scientific_name == "Caridina nilotica"), c("bodymass_min","bodymass_max")] <- c(0.01, 0.142) # https://www.sciencedirect.com/science/article/pii/S1474706512000824

# Macrozoobenthos -> arbitrary, went 0.5-2 just to be bigger than Zoobenthos
species_list[which(species_list$scientific_name == "Macrozoobenthos"), c("bodymass_min","bodymass_max")] <- c(0.5,2)

# Microtus oeconomus
species_list[which(species_list$scientific_name == "Microtus oeconomus"), c("bodymass_min","bodymass_max")] <- c(32.2, 43.0) # BODY MASS OF LATE QUATERNARY MAMMALS, Ecological Archives E084-094

# Geese (Anser brachyrhynchus, Branta leucopsis)
species_list[which(species_list$scientific_name == "Geese"), c("bodymass_min","bodymass_max")] <- c(1210,3400) # Wikipedia (https://en.wikipedia.org/wiki/Pink-footed_goose)(https://en.wikipedia.org/wiki/Barnacle_goose)

# Lemmings (Lemmus trimucronatus, Dicrostonyx groenlandicus)
species_list[which(species_list$scientific_name == "Lemmings"), c("bodymass_min","bodymass_max")] <- c(40, 69.82) # Eltontraits and Wikipedia for smallest (https://en.wikipedia.org/wiki/Northern_collared_lemming)

# Lemmus trimucronatus
species_list[which(species_list$scientific_name == "Lemmus trimucronatus"), c("bodymass_min","bodymass_max")] <- c(58, 113) # Mix Wikipedia (https://en.wikipedia.org/wiki/Canadian_lemming) and NatureServe (https://explorer.natureserve.org/Taxon/ELEMENT_GLOBAL.2.105072/Lemmus_trimucronatus)

# Dicrostonyx groenlandicus
species_list[which(species_list$scientific_name == "Dicrostonyx groenlandicus"), c("bodymass_min","bodymass_max")] <- c(30,112) # https://animaldiversity.org/accounts/Dicrostonyx_groenlandicus/

# Mustela erminea
species_list[which(species_list$scientific_name == "Mustela erminea"), c("bodymass_min","bodymass_max")] <- c(25,116) # https://animaldiversity.org/accounts/Mustela_erminea/

# Shorebirds
species_list[which(species_list$scientific_name == "Shorebirds"), c("bodymass_min","bodymass_max")] <- c(40.97, 250.00) # From Eltontraits birds

# Vulpes lagopus
species_list[which(species_list$scientific_name == "Vulpes lagopus"), c("bodymass_min","bodymass_max")] <- c(1400,9400) # Wikipedia (https://en.wikipedia.org/wiki/Arctic_fox)

# Passerines
species_list[which(species_list$scientific_name == "Passerines"), c("bodymass_min","bodymass_max")] <- c(20.68,42.20) # From Eltontraits birds

# Waterfowl, based the smallest and larges: Anas crecca, Cygnus columbianus
species_list[which(species_list$scientific_name == "Waterfowl"), c("bodymass_min","bodymass_max")] <- c(340,9600) # (https://en.wikipedia.org/wiki/Eurasian_teal)(https://en.wikipedia.org/wiki/Tundra_swan)

# Gulls (Larus hyperboreus, L. argentatus)
species_list[which(species_list$scientific_name == "Gulls"), c("bodymass_min","bodymass_max")] <- c(1090.99,1529.04) # From Eltontraits birds

# Plectrophenax nivalis
species_list[which(species_list$scientific_name == "Plectrophenax nivalis"), c("bodymass_min","bodymass_max")] <- c(26,50) # Wikipedia (https://fr.wikipedia.org/wiki/Bruant_des_neiges)

# Anser caerulescens atlantica
species_list[which(species_list$scientific_name == "Anser caerulescens atlanticus"), c("bodymass_min","bodymass_max")] <- c(2636.15,4500) # Mix Eltontraits birds  + Wikipedia (https://en.wikipedia.org/wiki/Snow_goose)

# Ptarmigan
species_list[which(species_list$scientific_name == "Ptarmigan"), c("bodymass_min","bodymass_max")] <- c(430,810) # Wikipedia(https://en.wikipedia.org/wiki/Willow_ptarmigan)(https://en.wikipedia.org/wiki/Rock_ptarmigan)

# Lepus arcticus
species_list[which(species_list$scientific_name == "Lepus arcticus"), c("bodymass_min","bodymass_max")] <- c(4000,5500) # Wikipedia(https://fr.wikipedia.org/wiki/Li%C3%A8vre_arctique)

# Ovibos moschatus
species_list[which(species_list$scientific_name == "Ovibos moschatus"), c("bodymass_min","bodymass_max")] <- c(180000,410000) # Wikipedia(https://en.wikipedia.org/wiki/Muskox)

# Rangifer tarandus
species_list[which(species_list$scientific_name == "Rangifer tarandus"), c("bodymass_min","bodymass_max")] <- c(100000,180000) # Wikipedia(https://fr.wikipedia.org/wiki/Rangifer_tarandus)

# Shrimps, Penaeus setiferus, P. aztecus
species_list[which(species_list$scientific_name == "Shrimps"), c("bodymass_min","bodymass_max")] <- c(0.7133*(1.2^2.940), 0.0084*(10.8^2.956)) # From sealifebase, mean of both shrimps

# Anchoa
species_list[which(species_list$scientific_name == "Anchoa"), c("bodymass_min","bodymass_max")] <- c(1.471769,20.833498) # From rfishbase L-W relationship

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
species_list[which(species_list$scientific_name == "Croaker"), c("bodymass_min","bodymass_max")] <- 
               rfishbase::length_weight(c("Bairdiella chrysura", "Bairdiella ronchus", "Micropogon undulatus")) |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Gerreidae
species_list[which(species_list$scientific_name == "Gerreidae"), c("bodymass_min","bodymass_max")] <- 
               rfishbase::length_weight(c("Eucinostomus melanopterus", "Diapterus rhombeus", "Diapterus auratus", "Eugerres plumieri")) |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Crustacea -> Clivanarius vittatus, decapod, amphipod
# Used 10.1017/S0025315409991408 for lowest (amphipoda) and the lowest of decpoda
species_list[which(species_list$scientific_name == "Crustacea"), c("bodymass_min","bodymass_max")] <- c(0.02, 15.94638) # To confirm

# Goby
species_list[which(species_list$scientific_name == "Goby"), c("bodymass_min","bodymass_max")] <- 
               rfishbase::length_weight(c("Bathygobius soporator", "Gobionellus boleosoma")) |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Scombridae
species_list[which(species_list$scientific_name == "Scombridae"), c("bodymass_min","bodymass_max")] <- 
               rfishbase::length_weight(c("Rastrelliger kanagurta", "Euthynnus affinis", "Alepes djedaba")) |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Small pelagic fish, Hilsa kelee
species_list[which(species_list$scientific_name == "Small pelagic fish"), c("bodymass_min","bodymass_max")] <-
               rfishbase::length_weight(c("Hilsa kelee")) |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Demersal fish
species_list[which(species_list$scientific_name == "Demersal fish"), c("bodymass_min","bodymass_max")] <- 
               rfishbase::length_weight(c("Pornadasys maculatus", "Otolithes ruber", "Johnius dussumieri", "Johniops sina")) |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Planktivorous haplochromines, https://www.biodiversitylibrary.org/part/119071 for the names
# Used Bayesian L-W relationships from fishbase (took the smallest of the five species)
# For the largest, used the asymptotic weight in the article
species_list[which(species_list$scientific_name == "Planktivorous haplochromines"), c("bodymass_min","bodymass_max")] <- c(3.06, 35) 

# Oreochromis -> Oreochromis esculentus from the article (major species)
species_list[which(species_list$scientific_name == "Oreochromis"), c("bodymass_min","bodymass_max")] <- 
               rfishbase::length_weight(c("Oreochromis esculentus")) |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Predatory haplochromines, https://www.biodiversitylibrary.org/part/119071
# Used Bayesian L-W relationships from fishbase (took the smallest of the five species)
# For the largest, used the asymptotic weight in the article
species_list[which(species_list$scientific_name == "Predatory haplochromines"), c("bodymass_min","bodymass_max")] <- c(0.00244*(10.4^2.81), 205)


# Crassostrea virginica, used the length-weight relationship from sealifebase and length from  WORMS https://www.marinespecies.org/aphia.php?p=taxdetails&id=140657#attributes
species_list[which(species_list$scientific_name == "Crassostrea virginica"), c("bodymass_min","bodymass_max")] <- c(0.021*(0.2^2.490), 0.021*(20^2.490))

# Crabs, Callinectes danae, C. boucorti, C. sapidus
species_list[which(species_list$scientific_name == "Crabs"), c("bodymass_min","bodymass_max")] <- 
               rfishbase::length_weight(c("Callinectes danae", "Callinectes boucorti", "Callinectes sapidus"), server = "sealifebase") |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Meiofauna -> arbitrary
species_list[which(species_list$scientific_name == "Meiofauna"), c("bodymass_min","bodymass_max")] <- c(0.005,0.5)

# Snout -> Synodontis and mormyrus
species_list[which(species_list$scientific_name == "Snout"), c("bodymass_min","bodymass_max")] <- c(16.39606,650) # lower-bound from L-W relationship, higher from article
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
species_list[which(species_list$scientific_name == "Snook"), c("bodymass_min","bodymass_max")] <-
               rfishbase::length_weight(c("Centropomus undecimalis", "Centropomus paralellus", "Centropomus poeyi")) |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Benthivorous haplochromines
# Took the same lowest boundery as for the phytophagous haplochromines
# Took the asymptotic weight in the article
species_list[which(species_list$scientific_name == "Benthivorous haplochromines"), c("bodymass_min","bodymass_max")] <- c(3.06, 40)

# Periphyton -> arbitrary
species_list[which(species_list$scientific_name == "Periphyton"), c("bodymass_min","bodymass_max")] <- c(0.05,0.5)

# Macrophytes -> arbitrary
species_list[which(species_list$scientific_name == "Macrophytes"), c("bodymass_min","bodymass_max")] <- c(0.5,1)

# Bivalvia -> Corbicula africana, Caelatura mossarnbicensis, Aspatharia wahlbergi and Mutela dubia
# Used the same as Mollusca
species_list[which(species_list$scientific_name == "Bivalvia"), c("bodymass_min","bodymass_max")] <- c(0.5,50)
               #rfishbase::length_weight(c("Corbicula africana", "Caelatura mossarnbicensis", "Aspatharia wahlbergi", "Mutela dubia"), server = "sealifebase") |>
               #dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               #dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               #dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               #dplyr::select(c("bodymass_min", "bodymass_max")) |>
               #colMeans(na.rm = TRUE)

# Chaoborus edulis
# Used the same as Zoobenthos, since Chaoborus is part of this group
species_list[which(species_list$scientific_name == "Chaoborus edulis"), c("bodymass_min","bodymass_max")] <- c(0.05,1)

# Large catfish
species_list[which(species_list$scientific_name == "Large catfish"), c("bodymass_min","bodymass_max")] <- c(79.04112,1973.48382)
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
species_list[which(species_list$scientific_name == "Small catfish"), c("bodymass_min","bodymass_max")] <- 
               rfishbase::length_weight(c("Ariopsis felis","Cathorops melanopus")) |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Neomysis integer -> used the shrimps part of the Decapoda group
species_list[which(species_list$scientific_name == "Neomysis integer"), c("bodymass_min","bodymass_max")] <- c(0.7133*(1.2^2.940), 0.0084*(10.8^2.956))

# Heterotrophic benthos -> same as Macrozoobenthos
species_list[which(species_list$scientific_name == "Heterotrophic benthos"), c("bodymass_min","bodymass_max")] <- c(0.5,2)

# Crustacea and Molluscs
species_list[which(species_list$scientific_name == "Crustacea and Molluscs"), c("bodymass_min","bodymass_max")] <-
               rfishbase::length_weight(c("Polynices mamilla","Murex ramosus","Scylla serrata","Eumarcia paupercula","Modiolus philippinarum"), server= "sealifebase") |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Palaemon longirostris -> used the shrimps part of the Decapoda group
species_list[which(species_list$scientific_name == "Palaemon longirostris"), c("bodymass_min","bodymass_max")] <- c(0.7133*(1.2^2.940), 0.0084*(10.8^2.956))


# Benthic predators -> Nereis diversicolor, Nephtys hombergii
species_list[which(species_list$scientific_name == "Benthic predators"), c("bodymass_min","bodymass_max")] <-
rfishbase::length_weight(c("Nereis diversicolor", "Nephtys hombergii"), server = "sealifebase") |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Crangon crangon
species_list[which(species_list$scientific_name == "Crangon crangon"), c("bodymass_min","bodymass_max")] <- 
rfishbase::length_weight("Crangon crangon", server = "sealifebase") |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Lichens
species_list[which(species_list$scientific_name == "Lichens"), c("bodymass_min","bodymass_max")] <- c(0.5,1) # To confirm

# Engraulicypris sardella
species_list[which(species_list$scientific_name == "Engraulicypris sardella"), c("bodymass_min","bodymass_max")] <- c(0.00254*(10^2.85),0.00785*(10^3.15))

# Copepoda, putting same as Zookplankton
species_list[which(species_list$scientific_name == "Copepoda"), c("bodymass_min","bodymass_max")] <- c(0.0001, 1)

# Stercorarius longicaudus
species_list[which(species_list$scientific_name == "Stercorarius longicaudus"), c("bodymass_min","bodymass_max")] <- c(230,444) # WIkipedia(https://en.wikipedia.org/wiki/Long-tailed_jaeger)

# Arthropoda -> Chironomidae, Muscidae, Aranea, Lepidoptera, Ichneumonidae, Carabidae
# Arbitrary
species_list[which(species_list$scientific_name == "Arthropoda"), c("bodymass_min","bodymass_max")] <- c(0.5,1)

# Alepisaurus -> A. minutus and A. ferox
species_list[which(species_list$scientific_name == "Alepisaurus"), c("bodymass_min","bodymass_max")] <- 
               rfishbase::length_weight(c("Alepisaurus minutus","Alepisaurus ferox"), server = "fishbase") |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Synodontis zambezensis -> Bayesian L-W from fishbase (https://www.fishbase.ca/summary/Synodontis-zambezensis.html)
species_list[which(species_list$scientific_name == "Synodontis zambezensis"), c("bodymass_min","bodymass_max")] <- c(0.00509*(15^2.84), 0.02256*(15^3.22))

# Limnothrissa miodon -> Bayesian L-W from fishbase (https://www.fishbase.ca/summary/Limnothrissa-miodon.html)
species_list[which(species_list$scientific_name == "Limnothrissa miodon"), c("bodymass_min","bodymass_max")] <- c(0.00405*(10^2.84), 0.01297*(10^3.14))

# Cichlids -> Tilapia rendalli, Serranochromis codringtoni, Serranochromis macrocephalus, Oreochromis mortimeri
species_list[which(species_list$scientific_name == "Cichlids"), c("bodymass_min","bodymass_max")] <-
               rfishbase::validate_names(c("Tilapia rendalli", "Serranochromis codringtoni", "Serranochromis macrocephalus", "Oreochromis mortimeri")) |>
               rfishbase::length_weight() |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Stolothrissa tanganicae -> Bayesian L-W from fishbase (https://www.fishbase.de/summary/Stolothrissa-tanganicae.html)
species_list[which(species_list$scientific_name == "Stolothrissa tanganicae"), c("bodymass_min","bodymass_max")] <- c(0.00230*(7^2.90), 0.01313*(7^3.32))

# Benthic fish -> Labeo horie, Barbus bynni, Citharinus citharis and  Distichodus nefasch
species_list[which(species_list$scientific_name == "Benthic fish"), c("bodymass_min","bodymass_max")] <-
               rfishbase::validate_names(c("Labeo horie", "Barbus bynni", "Citharinus citharus", "Distichodus nefasch")) |>
               rfishbase::length_weight() |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Lates stappersii -> Bayesian L-W from fishbase (Bayesian L-W from fishbase)
species_list[which(species_list$scientific_name == "Lates stappersii"), c("bodymass_min","bodymass_max")] <- c(0.00628*(45^2.92 ), 0.01053*(45^3.06))

# Lates
species_list[which(species_list$scientific_name == "Lates"), c("bodymass_min","bodymass_max")] <-
               rfishbase::validate_names(c("Lates mariae", "Lates microlepis", "Lates angustifrons","Lates niloticus", "Lates longispinis")) |>
               rfishbase::length_weight() |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Copadichromis azureus -> Bayesian L-W from fishbase (https://fishbase.in/summary/Copadichromis-azureus.html)
species_list[which(species_list$scientific_name == "Copadichromis azureus"), c("bodymass_min","bodymass_max")] <- c(0.00687*(14.6^2.79), 0.03186*(14.6^3.15))
               #rfishbase::validate_names(c("Copadichromis azureus")) |>
               #rfishbase::length_weight() |>
               #dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               #dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               #dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               #dplyr::select(c("bodymass_min", "bodymass_max")) |>
               #colMeans(na.rm = TRUE)

# Arius
species_list[which(species_list$scientific_name == "Arius"), c("bodymass_min","bodymass_max")] <- 
               rfishbase::validate_names(c("Arius felis", "Arius melanopusopadichromis azureus")) |>
               rfishbase::length_weight() |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Bubo scandiacus (https://fr.wikipedia.org/wiki/Harfang_des_neiges)
species_list[which(species_list$scientific_name == "Bubo scandiacus"), c("bodymass_min","bodymass_max")] <- c(1000,2500)

# Buteo lagopus (https://en.wikipedia.org/wiki/Rough-legged_buzzard)
species_list[which(species_list$scientific_name == "Buteo lagopus"), c("bodymass_min","bodymass_max")] <- c(600,1660)

#  Canis lupus arctos (https://fr.wikipedia.org/wiki/Loup_arctique)
species_list[which(species_list$scientific_name == "Canis lupus arctos"), c("bodymass_min","bodymass_max")] <- c(45000, 80000)

# Carangidae
species_list[which(species_list$scientific_name == "Carangidae"), c("bodymass_min","bodymass_max")] <- 
               rfishbase::validate_names(c("Caranx amblyrhynchus", "Caranx hippos", "Caranx latus", "Caranx crysos")) |>
               rfishbase::length_weight() |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Diplotaxodon -> Bayesian L-W from fishbase
# took the shortest and longest of these species, same parameters for Bayesian L-W:
# Diplotaxodon macrops, Diplotaxodon limnothrissa, Diplotaxodon greenwoodi, Diplotaxodon ecclesi, Diplotaxodon argenteus, Diplotaxodon apogon, Diplotaxodon aeneus
species_list[which(species_list$scientific_name == "Diplotaxodon"), c("bodymass_min","bodymass_max")] <- c(0.00244*(11^2.81), 0.04107*(24.7^2.81))

# Falco -> Falco peregrinus, F. rusticolus (https://en.wikipedia.org/wiki/Peregrine_falcon) ()
species_list[which(species_list$scientific_name == "Falco"), c("bodymass_min","bodymass_max")] <- c(330, 2100)

# Falco peregrinus 330-1500
species_list[which(species_list$scientific_name == "Falco peregrinus"), c("bodymass_min","bodymass_max")] <- c(330,1500)

# Gulo gulo (https://fr.wikipedia.org/wiki/Gulo_gulo)
species_list[which(species_list$scientific_name == "Gulo gulo"), c("bodymass_min","bodymass_max")] <- c(8000,12000)

# Jaegers -> Stercorarius longicaudus, S. parasiticus (https://en.wikipedia.org/wiki/Parasitic_jaeger)(https://en.wikipedia.org/wiki/Long-tailed_jaeger)
species_list[which(species_list$scientific_name == "Jaegers"), c("bodymass_min","bodymass_max")] <- c(230, 650)

# Larus hyperboreus (https://en.wikipedia.org/wiki/Glaucous_gull)
species_list[which(species_list$scientific_name == "Larus hyperboreus"), c("bodymass_min","bodymass_max")] <- c(960,2700)

# Opsaridium microcephalum (https://www.fishbase.ca/summary/Opsaridium-microcephalum.html)
species_list[which(species_list$scientific_name == "Opsaridium microcephalum"), c("bodymass_min","bodymass_max")] <- c(0.00313*(23.7^2.86), 0.01602*(23.7^3.24))

# Other fish2
species_list[which(species_list$scientific_name == "Other fish2"), c("bodymass_min","bodymass_max")] <- 
               rfishbase::validate_names(c("Trisopterus luscus", "Pleuronectes platessa", "Clupea harengus", "Sprattus sprattus", "Solea vulgaris", "Pomatoschistus microps")) |>
               rfishbase::length_weight() |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Rhamphochromis longiceps -> Bayesian L-W from fishbase (https://www.fishbase.ca/summary/Rhamphochromis-longiceps.html)
species_list[which(species_list$scientific_name == "Rhamphochromis longiceps"), c("bodymass_min","bodymass_max")] <- c(0.00244*(28^2.81), 0.04107*(28^3.27))

# Seabirds
# Calidris alpina,Haemotopus ostrealegus,Numenius aquatus,Fulica atra,Anas crecca,Anas plathyrhynchus,Larus ridibundus,Larus argentus, Podiceps cristatus, Phalocrocorax carbo
species_list[which(species_list$scientific_name == "Seabirds"), c("bodymass_min","bodymass_max")] <- c(48,3700)

# Fishing birds
# Fishing eagles (aliaeetus vocifer, https://fr.wikipedia.org/wiki/Pygargue_vocifer), kingfishers (used belted kingfisher, https://en.wikipedia.org/wiki/Belted_kingfisher), 
#cormorants (https://fr.wikipedia.org/wiki/Cormoran), pelicans (https://fr.wikipedia.org/wiki/P%C3%A9lican_gris)
species_list[which(species_list$scientific_name == "Fishing birds"), c("bodymass_min","bodymass_max")] <- c(113,7000)

# Sheephead -> Archosargus probatocephalus, Lagodon romboides
species_list[which(species_list$scientific_name == "Sheephead"), c("bodymass_min","bodymass_max")] <-
               rfishbase::validate_names(c("Archosargus probatocephalus", "Lagodon romboides")) |>
               rfishbase::length_weight() |>
               dplyr::select(c("Species","LengthMin","LengthMax","a","b")) |>
               dplyr::mutate(LengthMin = as.numeric(LengthMin), LengthMax = as.numeric(LengthMax)) |>
               dplyr::mutate(bodymass_min = a * LengthMin^b, bodymass_max = a * LengthMax^b) |>
               dplyr::select(c("bodymass_min", "bodymass_max")) |>
               colMeans(na.rm = TRUE)

# Stercorarius parasiticus (https://en.wikipedia.org/wiki/Parasitic_jaeger)
species_list[which(species_list$scientific_name == "Stercorarius parasiticus"), c("bodymass_min","bodymass_max")] <- c(300,650)

# Ursus arctos (https://fr.wikipedia.org/wiki/Ours_brun)
species_list[which(species_list$scientific_name == "Ursus arctos"), c("bodymass_min","bodymass_max")] <- c(130000, 700000)

# Vulpes vulpes (https://fr.wikipedia.org/wiki/Renard_roux)
species_list[which(species_list$scientific_name == "Vulpes vulpes"), c("bodymass_min","bodymass_max")] <- c(2200,14000)

# Check remaining missing
# Check which species are NAs for bodymass
missing_mass2 <- species_list[which(is.na(species_list$bodymass_min & species_list$bodymass_max)),]

# Save the file
saveRDS(species_list, "data/intermediate/new/species_traits_new.RDS")
