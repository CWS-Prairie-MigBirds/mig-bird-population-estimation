###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_
### CHECKING DATA AVAILABILITY FOR SPECIES OF INTEREST AND CREATED
### LOOK UP TABLES AND SPATIAL EXTENTS FOR ALL DATA SOURCES
###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_
##
## Objective:
## 1. Check for which species of interest we have (1) eBird Status & Trends data, (2) BAM
## density models, (3) CGAM density models, (4) DUC density models.
## 2. Create lookup tables with population estimates from all available sources
## 3. Create spatial extent objects for each model sources, inlcuded strata, when necessary
##
## Written by: Jenny Munoz and Barry Robinson
## Last updated: April 2026
###
###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_

# ================================================================
# 0) SETUP
# ================================================================

# Install packages (run once only if needed)
packages <- c(
  "tidyverse", "janitor", "glue", "fs", "png",
  "viridis", "scales", "fields", "readr",
  "rnaturalearth", "sf", "raster", "ebirdst",
  "rmapshaper", "terra", "BAMexploreR"
)

# Identify which ones are not yet installed
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Install only the missing ones
if(length(new_packages) >0) {
  install.packages(new_packages)
}

# Load libraries
library(rnaturalearth)
library(BAMexploreR)
library(terra)
library(sf)
library(ebirdst)
library(readxl)
library(janitor)
library(dplyr)
library(stringr)
options(scipen = 999)


# ================================================================
# 1) CREATE MASTER LIST OF SPECIES AND SOURCES OF DISTRIBUTION 
# MODELS FOR EACH
# ================================================================
# Start with eBird because it has the longest species list
# Select key fields from eBird runs table
ebirdst_runs_selected <- ebirdst_runs %>%
  dplyr::select(
    species_code,
    scientific_name,
    common_name,
    breeding_start,
    breeding_end,
    breeding_quality
  )

#create simplified eBird table to combine with NAWCA species table below
eBirdspp <- data.frame(common_name = ebirdst_runs_selected$common_name,
                       eBird = "Yes") %>%
  mutate(common_name = recode(common_name, 
                              "Northern/Southern House Wren" = "House Wren"))

#get list of species with BAM v4 and v5 models
bam <- data.frame(common_name = bam_spp_list("v4", "commonName"),
                     BAMv4 = "Yes") %>%
  mutate(common_name = recode(common_name,
                              "Gray Jay" = "Canada Jay")) %>%
  full_join(data.frame(common_name = bam_spp_list("v5", "commonName"),
                       BAMv5 = "Yes")) %>%
  mutate(BAMv5 = ifelse(is.na(BAMv5), "No", BAMv5),
         BAMv4 = ifelse(is.na(BAMv4), "No", BAMv4))

#create list of species with CGAM models
cgam <- data.frame(common_name = c("Baird's Sparrow", "Bobolink", "Brewer's Sparrow", "Chestnut-collared Longspur", "Clay-colored Sparrow", 
                                   "Eastern Kingbird", "Grasshopper Sparrow", "Horned Lark", "Lark Bunting", "Long-billed Curlew", "LeConte's Sparrow",
                                   "Loggerhead Shrike", "Marbled Godwit", "Savannah Sparrow", "Sedge Wren", "Sprague's Pipit", "Thick-billed Longspur", 
                                   "Upland Sandpiper", "Vesper Sparrow", "Western Kingbird", "Western Meadowlark", "Willet"),
                   CGAMv1 = "Yes")

#create list of species with DUC models
duc <- data.frame(common_name = c("Blue-winged Teal", "Canvasback", "Gadwall", "Mallard", "Northern Pintail", "Northern Shoveler", "Redhead"),
                  DUC = "Yes")

#combine all model sources into a single table
species_models <- full_join(eBirdspp, bam) %>%
  full_join(cgam) %>%
  full_join(duc) %>%
  mutate(across(everything(), ~coalesce(.x, "No")))

#export species distribution model table
dir.create("LookupData")
write.csv(species_models, "LookupData/sdm_speciesList.csv", row.names = F)

# View full list if needed
# View(ebirdst_runs_selected)

# ================================================================
# 2) CREATE MASTER LIST OF SPECIES AND SOURCES OF POPULATION 
# ESTIMATES
# ================================================================
dir.create("data/PopEsts")
#Download and manipulate ACAD population estimates
download.file("https://api.bcr.eco/pif/2023/acad/global/download",
              destfile = "data/PopEsts/Global_ACAD_2024_05_23.xlsx",
              mode = "wb")
acad <- read_excel("data/PopEsts/Global_ACAD_2024_05_23.xlsx", sheet = 1) %>%
  janitor::clean_names() %>% #remove spaces and uppercase from colnames
  dplyr::filter(canada == 1 | usa == 1) %>% #remove species that don't occur in Canada or USA
  dplyr::select(common_name, global_pop_size_number, pop_size_us_ca_number) %>% #selection population estimate columns only
  dplyr::rename(acad_global = global_pop_size_number, #change to shorter names
         acad_uscan = pop_size_us_ca_number)



#Download and manipulate PIF Population estimates
download.file("https://api.bcr.eco/pif/2023/ped/bcr/province/download",
              destfile = "data/PopEsts/PopEsts_BCRxProvState_2020_04_24.xlsx",
              mode = "wb")
pif_reg <- read_excel("data/PopEsts/PopEsts_BCRxProvState_2020_04_24.xlsx") %>%
  janitor::clean_names() %>% #remove spaces and uppercase from colnames
  dplyr::select(english_name, bcr, province_state_territory, country, population_estimate) %>% #selection needed columns
  dplyr::rename(common_name = english_name, #create shorter names
                prov_state = province_state_territory,
                pop_est = population_estimate) %>%
  mutate(stratum = paste(bcr, prov_state, sep = "_")) %>%
  select(common_name, stratum, country, pop_est)

# download.file("https://api.bcr.eco/pif/2023/ped/global/download",
#               destfile = "data/PopEsts/PopEsts_Global_2020_04_29.xlsx",
#               mode = "wb")
# pif_glo <- read_excel("data/PopEsts/PopEsts_Global_2020_04_29.xlsx") %>%
#   janitor::clean_names() %>% #remove spaces and uppercase from colnames
#   dplyr::select(english_name, population_estimate_global) %>% #selection population estimate columns only
#   dplyr::rename(common_name = english_name,
#                 pif_global = population_estimate_global) #change to shorter names
    
#Download and manipulate USFWS waterfowl population estimates for Traditional survey area and 4-letter species codes
#ONLY RUN THIS SECTION ONCE
############################
# download.file("https://iris.fws.gov/APPS/ServCat/DownloadFile/277349",
#               destfile = "data/PopEsts/usfws_waterfowl_pops_2025.zip",
#               mode = "wb")
# download.file("https://www.birdpop.org/docs/misc/IBPAOU.zip",
#               destfile = "data/birdCodes.zip",
#               mode = "wb")
# unzip("data/PopEsts/usfws_waterfowl_pops_2025.zip", exdir = "data/PopEsts")
# unzip("data/birdCodes.zip", exdir = "data/PopEsts")
###########################
usfws <- read.csv("data/PopEsts/WBPHS_Traditional_Area_Stratum_Estimates/wbphs_traditionalarea_estimates_forDistribution.csv") %>%
 mutate(survey_species = case_match(survey_species,
                                    "CAGO" ~ "CANG",
                                    .default = survey_species)) %>%
  filter(survey_species != "POND" & survey_year == 2025) %>%
  rename(pop_est = estimate)


spCodes <- read.csv("data/PopEsts/IBP-AOS-list25.csv") %>%
  select(SPEC, COMMONNAME) %>%
  rename(survey_species = SPEC,
         common_name = COMMONNAME) %>%
  rbind(data.frame("survey_species" = c("SCAU", "SWAN", "MERG", "GOLD", "SCOT"),
                   "common_name" = c("Scaup spp", "Swan spp", "Merganser spp", "Goldeneye spp", "Scoter spp")))

usfws_exp <- usfws %>%
  left_join(spCodes) %>%
  select(common_name, stratum, pop_est)

file.remove(c("data/PopEsts/usfws_waterfowl_pops_2025.zip", "data/birdCodes.zip"))

#Obtain USFWS waterfowl population estimates for Eastern Survey Area and combine with Traditional survey area
eastern <- read.csv("data/PopEsts/eastern-composite-model-BPOP-2025.csv") |>
  mutate(survey_species = toupper(MASAlpha)) |>
  rename(stratum = Region,
         pop_est = Est) |>
  filter(Yr == 2025) |>
  left_join(spCodes) |>
  select(common_name, stratum, pop_est) |>
  na.omit()

usfws_exp <- rbind(usfws_exp, eastern)


#Create lookup table for sources population estimates
dir.create("data/PopEsts/modified")
acadsp <- data.frame(common_name = unique(acad$common_name),
                     acad = "Yes")
pifsp <- data.frame(common_name = unique(pif_reg$common_name),
                    pif_reg = "Yes")
usfwssp <- data.frame(common_name = unique(usfws_exp$common_name),
                      fws_reg = "Yes")
species_pop <- full_join(acadsp, pifsp) %>%
  full_join(usfwssp) %>%
  mutate(across(everything(), ~coalesce(.x, "No")))

#save populations estimates and lookup table
write.csv(species_pop, "LookupData/popEst_speciesList.csv", row.names = F)
write.csv(usfws_exp, "LookupData/usfws.csv", row.names = F)
write.csv(acad, "LookupData/acad.csv", row.names = F)
write.csv(pif_reg, "LookupData/pif.csv", row.names = F)

# ================================================================
# 3) CREATE SPATIAL EXTENT LAYERS FOR EACH DISTRIBUTION MODEL SOURCE
# ================================================================
dir.create("data/spatial")
#BAM spatial extent map
#These can be extracted directly from the BAM package, but loading here to reproject all other layers with CRS of BAM
#BAM uses CONUS Albers, which is a good projection for NA shapefiles
bamv4 <- bam_map_bcr(version = "v4")[[1]]$shp
bamv5 <- bam_map_bcr(version = "v5")[[1]]$shp
bamCRS <- crs(bamv5)

#BCR state/province boundaries
#create lookup table to get province/state abbreviations. Needed to link with PIF regional population estimates
us <- tibble(PROVINCE_S = toupper(state.name),
             prov_state = state.abb)
ca <- tibble(PROVINCE_S = c("ALBERTA", "BRITISH COLUMBIA", "MANITOBA", "NEW BRUNSWICK",
                            "NEWFOUNDLAND", "NOVA SCOTIA", "ONTARIO",
                            "PRINCE EDWARD ISLAND", "QUEBEC", "SASKATCHEWAN",
                            "NORTHWEST TERRITORIES", "NUNAVUT", "YUKON"),
             prov_state = c("AB", "BC", "MB", "NB", "NL", "NS", "ON", "PE", "QC", "SK",
                            "NT", "NU", "YT"))
lookup <- bind_rows(us, ca)
rm(ca, us)

#ONLY RUN THIS SECTION ONCE
############################
# download.file("https://services1.arcgis.com/d5M16PKlQTMEVyua/arcgis/rest/services/BCR_terrestrial_political_divisions/FeatureServer/replicafilescache/BCR_terrestrial_political_divisions_-3846708849115759799.zip",
#               destfile = "data/spatial/bcr.zip",
#               mode = "wb")
# unzip("data/spatial/bcr.zip", exdir = "data/spatial")
# file.remove("data/spatial/bcr.zip")
# st_layers("data/spatial/c464597e-d654-494e-b6d1-8bafde6b83c8.gdb")
###########################

bcr <- st_read(dsn = "data/spatial/c464597e-d654-494e-b6d1-8bafde6b83c8.gdb", layer = "BCR_terrestrial_by_political_divisions") %>%
  select(bcr_label, PROVINCE_S, COUNTRY, SHAPE_Area) %>%
  mutate(bcr_label = str_remove_all(bcr_label, "[A-Za-z]")) %>%
  group_by(bcr_label, PROVINCE_S) %>%
  summarise(COUNTRY = first(COUNTRY),
            SHAPE_Area = sum(SHAPE_Area, na.rm = TRUE),  # aggregate area
            .groups = "drop") %>%
  left_join(lookup) %>%
  filter(COUNTRY == "USA" | COUNTRY == "CANADA") %>%
  rename(bcr = bcr_label) %>%
  mutate(stratum = paste(bcr, prov_state, sep = "_")) %>%
  select(stratum, COUNTRY, SHAPE_Area) %>%
  st_transform(crs = bamCRS)

#crop to exclude Hawaii
bb <- st_bbox(bcr)
bb["xmin"] <- -5000000
bb_poly <- st_as_sfc(bb)
bcr <- st_intersection(bcr, bb_poly)
plot(bcr)


#USFWS Traditional Survey Area Strata
#ONLY RUN THIS SECTION ONCE
############################
# download.file("https://iris.fws.gov/APPS/ServCat/DownloadFile/241521",
#               destfile = "data/WBPHS_stratum_boundaries.zip",
#               mode = "wb")
# dir.create("data/spatial/WaterfowlStrata")
# unzip("data/WBPHS_stratum_boundaries.zip", exdir = "data/spatial/WaterfowlStrata")
# file.remove("data/WBPHS_stratum_boundaries.zip")
############################
wfStrata <- st_read("data/spatial/WaterfowlStrata/WBPHS_Stratum_Boundaries.shp") %>%
  group_by(stratum) %>%
  summarize(geometry = st_union(geometry)) %>%
  filter(stratum %in% unique(usfws_exp$stratum))
  st_transform(crs = bamCRS)

#CGAM model boundary
cgam <- rast("data/spatial/CGAM_ex.tif") %>%
  as.polygons(dissolve = T) %>%
  st_as_sf() %>%
  mutate(CGAM_ex = 1) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_transform(crs = bamCRS)

#DUC density models
duc <- rast("data/spatial/ducRasters/AGWT_perSQK.tif") %>%
  as.polygons(dissolve = T) %>%
  st_as_sf()%>%
  mutate(DUC_ex = 1) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_transform(crs = bamCRS)
  

#export all layers to a single gpkg file

st_write(bcr, dsn = "LookupData/modelExtents.gpkg", layer = "pif_reg")
st_write(bamv4, dsn = "LookupData/modelExtents.gpkg", layer = "bamv4", append = T)
st_write(bamv5, dsn = "LookupData/modelExtents.gpkg", layer = "bamv5", append = T)
st_write(wfStrata, dsn = "LookupData/modelExtents.gpkg", layer = "usfws", append = T)
st_write(cgam, dsn = "LookupData/modelExtents.gpkg", layer = "cgam", append = T)
st_write(duc, dsn = "LookupData/modelExtents.gpkg", layer = "duc", append = T)

  











########################################################################################
#Likely not needed
##########################################################################################



#Canada/US boundary (May not need this because I can get it from the BCR layer below)
us <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name != "Hawaii") %>%         # remove Hawaii
  select(name) %>%
  st_transform(5070)
can_us <- ne_states(country = "Canada", returnclass = "sf") %>%
  select(name) %>%
  st_transform(5070) %>%
  rbind(us) %>%
  st_write("data/spatial/can_usa.shp")

# List of NAWCA species
nawca_species <- nawca_list %>%
  dplyr::select(
    common_name,
    global_or_american_predominantly,
    ACAD,
    PIF
  )

#join species list for each model type with nawca species list
nawca_species_models <- left_join(nawca_species, eBirdspp) %>%
  left_join(bam) %>%
  left_join(cgam) %>%
  left_join(duc) %>%
  mutate(across(everything(), ~coalesce(.x, "No")))

# Match NAWCA species to eBird data
match_nawca_species_ebird <- nawca_species %>%
  left_join(ebirdst_runs_selected)

View(match_nawca_species_ebird)

#export updated NAWCA species table with model sources
write.csv(nawca_species_models, "data/nawca_species_models.csv", row.names = F)

# ================================================================
# 4) IDENTIFY SPECIES WITH NO POPULATION ESTIMATE AND/OR MODEL SOURCE
# ================================================================

missing_species_models <- nawca_species_models %>%
  filter(if_all(all_of(c("eBird", "BAMv4", "BAMv5", "CGAMv1", "DUC")), ~ .x == "No")) %>%
  pull(common_name)

missing_species_models #only 3

missing_species_pop <- nawca_species_models %>%
  filter(if_all(all_of(c("ACAD", "PIF")), ~ .x == "No")) %>%
  pull(common_name)

missing_species_pop #all have population estimates

missing_species_nawca_ebird <- match_nawca_species_ebird %>%
  filter(is.na(species_code)) %>%
  dplyr::select(common_name, scientific_name) %>%
  mutate(ebird_data = "no")




# ------------------------------------------------
# List of species without eBird Status & Trends data
# ------------------------------------------------
#
# 1  Ancient Murrelet
# 2  Black-crowned Night-Heron
# 3  Cassin's Auklet
# 4  Common Murre
# 5  Hudsonian Whimbrel
# 6  Masked Duck
# 7  Northern Goshawk
# 8  Red-necked Phalarope
# 9  White-rumped Sandpiper
# 10 Yellow Rail
#

# ================================================================
# 5) MANUAL TAXONOMY CHECK
# ================================================================

# Inspect full eBird species table
View(ebirdst::ebirdst_runs)

# Notes on taxonomy or data availability:
#
# Ancient Murrelet            -> No data available
# Black-crowned Night-Heron   -> Name mismatch (hyphen removed manually in NAWCA list)
# Cassin's Auklet             -> No data available
# Common Murre                -> No data available
# Hudsonian Whimbrel          -> Needs verification
# Masked Duck                 -> Needs verification
# Northern Goshawk            -> Needs verification
# Red-necked Phalarope        -> Needs verification
# White-rumped Sandpiper      -> Needs verification
# Yellow Rail                 -> Needs verification
#
# ------------------------------------------------
