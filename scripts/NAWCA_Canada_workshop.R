#Script to create examples for NAWCA Guidance Workshop
library(sf)
library(rnaturalearth)
library(dplyr)
library(terra)
library(ggplot2)
source("functions/Function_PopEsts.R")

#Import items needed for all examples
priority_spp <- read.csv("LookupData/nawca_acad_species_match.csv")
priority_spp$NAWCA_species
can <- ne_states(country = "Canada", returnclass = "sf") %>%
  select(name) %>%
  vect() %>%
  project("EPSG:8857")


#PBHJV
#####################################################################
pbhjvpoly <- st_read("data/spatial/polygons/PBHJV_ex1.shp") %>%
  list()
names(pbhjvpoly) <- pbhjvpoly[[1]]$NAME_E
pbhjvsp <- c("Surf Scoter", "White-winged Scoter", "Belted Kingfisher", "Townsend's Warbler")
pbhjv_est <- popEsts(pbhjvsp, pbhjvpoly)
write.csv(pbhjv_est, "Output/PBHJV_ex.csv", row.names = F)

#reproject eBird rasters for making maps in ArcPro
BEKI <- terra::rast("data/spatial/eBirdRasters/2023/belkin1/seasonal/belkin1_abundance_seasonal_mean_3km_2023.tif") %>%
  crop(can, mask = T) %>%
  terra::project("EPSG:3347")  # Canada Albers)
plot(BEKI)
dir.create("data/spatial/eBirdRasters/reprojected")
writeRaster(BEKI, "data/spatial/eBirdRasters/reprojected/BEKI_ebird.tif")
######################################################################

#CIJV
######################################################################
cijvpoly <- st_read("data/spatial/polygons/CIJV_ex1.shp") %>%
  list()
names(cijvpoly) <- cijvpoly[[1]]$NAME_E
cijvsp <- c("Bufflehead", "Killdeer", "Olive-sided Flycatcher")
cijv_est <- popEsts(cijvsp, cijvpoly)
write.csv(cijv_est, "Output/CIJV_ex.csv", row.names = F)

#reproject eBird rasters for making maps in ArcPro
OSFL <- terra::rast("data/spatial/eBirdRasters/2023/olsfly/seasonal/olsfly_abundance_seasonal_mean_3km_2023.tif") %>%
  crop(can, mask = T) %>%
  terra::project("EPSG:3347")  # Canada Albers)
plot(OSFL)
writeRaster(OSFL, "data/spatial/eBirdRasters/reprojected/OSFL_ebird.tif")
######################################################################


#EHVJ
######################################################################
ehjvpoly <- st_read("data/spatial/polygons/ehjv_ex1.shp") %>%
  list()
names(ehjvpoly) <- ehjvpoly[[1]]$NAME_E
ehjvsp <- c("American Black Duck", "Common Eider", "Lincoln's Sparrow", "Olive-sided Flycatcher")
ehjv_est <- popEsts(ehjvsp, ehjvpoly)
write.csv(ehjv_est, "Output/ehjv_ex.csv", row.names = F)

ehjvpoly2 <- st_read("data/spatial/polygons/ehjv_ex2.shp") %>%
  list()
names(ehjvpoly2) <- ehjvpoly2[[1]]$NAME_E
ehjv_est <- popEsts(ehjvsp, ehjvpoly2)

#reproject eBird rasters for making maps in ArcPro
OSFL <- terra::rast("data/spatial/eBirdRasters/2023/olsfly/seasonal/olsfly_abundance_seasonal_mean_3km_2023.tif") %>%
  crop(can, mask = T) %>%
  terra::project("EPSG:3347")  # Canada Albers)
plot(OSFL)
writeRaster(OSFL, "data/spatial/eBirdRasters/reprojected/OSFL_ebird.tif")
########################################################################

#PHJV
########################################################################
phjvpoly <- st_read("data/spatial/polygons/phjv_ex1.shp") %>%
  list()
names(phjvpoly) <- phjvpoly[[1]]$Landscap_1
phjvsp <- c("Northern Pintail", "Blue-winged Teal", "Long-billed Curlew", "Bobolink", "Marbled Godwit")
phjv_est <- popEsts(phjvsp, phjvpoly)
write.csv(phjv_est, "Output/phjv_ex.csv", row.names = F)




ggplot(wfStrata) +
  geom_sf(fill = "lightblue", color = "black") +
  geom_sf_text(aes(label = stratum), size = 3) +
  theme_minimal()




















