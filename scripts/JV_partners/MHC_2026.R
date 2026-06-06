#Script to implement function for MHC

library(sf)
library(dplyr)
source("functions/Function_PopEsts.R")
#load target landscape polygon
polysf <- sf::st_read(dsn = "data/spatial/polygons/PHJV_Target_Landscapes_2026_Final.shp") %>%
  filter(Prov == "MB")
polys <- split(polysf, seq_len(nrow(polysf))) #split individual polygon into a list
names(polys) <- polysf$Name #assign names to the list of polygons based on the "NAME_E" column from the attribute table

#There's a problem with the vertices of one of the MB polygons
sf::sf_use_s2(FALSE)
polys <- lapply(polys, function(x) {
  return(st_make_valid(x))
})

#load target landscape buffer polygon
polysf <- sf::st_read(dsn = "data/spatial/polygons/PHJV_Target_Landscapes_2026_FinalBuffer.shp")
polys <- list("TargetLandscapeBuffer" = polysf)

#There's a problem with the vertices of one of the MB polygons
sf::sf_use_s2(FALSE)
polys <- lapply(polys, function(x) {
  return(st_make_valid(x))
})

sf::sf_use_s2(TRUE)

#load example tracts of land
polyList <- list.files("data/spatial/polygons/MHC_tracts/Projects5.64Buffer", pattern = "\\.shp$", full.names = T)
polyNames <- list.files("data/spatial/polygons/MHC_tracts/Projects5.64Buffer", pattern = "\\.shp$") %>%
  sub("\\.shp$", "", .)
polys <- lapply(polyList, function(x) {
  tmp <- st_read(x) |>
    summarise()
  return(tmp)
})
names(polys) <- polyNames

#plot to check the polygons
lapply(polys, plot)
#most of these are very small, so need to relax min size restriction
lapply(polys, st_area) |>
  unlist() |>
  min()


species1 <- c("Northern Pintail","Lesser Scaup","Mallard","Snow Goose","Tundra Swan")
pop_ests1 <- popEsts(species1, polys)

species2 <- c("Trumpeter Swan","American Wigeon","Greater Scaup","Blue-winged Teal")
pop_ests2 <- popEsts(species2, polys)

species3 <- c("Green-winged Teal","Canvasback","Bufflehead","Goldeneye","Gadwall","Canada Goose")
pop_ests3 <- popEsts(species3, polys)

species4 <- c("Cackling Goose","Redhead","Northern Shoveler","Wood Duck")
pop_ests4 <- popEsts(species4, polys)

species5 <- c("American Coot","Black Tern","Bobolink","Franklin's Gull","Ring-necked Duck","Western Grebe")
pop_ests5 <- popEsts(species5, polys)

species6 <- c("Wilson's Phalarope","Short-eared Owl","Wilson's Snipe","American Woodcock")
pop_ests6 <- popEsts(species6, polys)

species7 <- c("American Avocet","Bank Swallow","Barn Swallow","Buff-breasted Sandpiper", "Marbled Godwit")
pop_ests7 <- popEsts(species7, polys)

species8 <- c("Greater Yellowlegs","Lesser Yellowlegs","Pectoral Sandpiper","Stilt Sandpiper","Semipalmated Sandpiper","Baird's Sandpiper")
pop_ests8 <- popEsts(species8, polys)

pop_ests9 <- popEsts(c("Semipalmated Sandpiper"), polys)


results <- rbind(pop_ests1, pop_ests2, pop_ests3, pop_ests4, pop_ests5, pop_ests6, pop_ests7, pop_ests8, pop_ests9)

write.csv(results, "Output/MHC_TargetLandscapes.csv", row.names = F)
write.csv(results_buffer, "Output/MHC_KillarneyBuffer.csv", row.names = F)
write.csv(results, "Output/ExampleTracts.csv", row.names = F)

write.csv(results, "Output/MHC_BufferedPolygons2.csv", row.names = F)




pop_est9 <- popEsts(c("Semipalmated Sandpiper"), polys)

