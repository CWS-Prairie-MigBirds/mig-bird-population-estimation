##########################################################################################################
#This script provides an example implementation of the Population Estimation function (Function_PopEsts.R)
#Written by: Jenny Munoz and Barry Robinson
#Last updated: April 22, 2026.
##########################################################################################################

#########################################################################################################
# 1). Install any required packages that you don't already have (no need to load all of them)
required_pkgs <- c("sf", "dplyr", "BAMexploreR", "ebirdst", "terra", "osfr", "purrr")
missing <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  install.packages(missing)
}
#########################################################################################################



#########################################################################################################
# 2). Set up files and access key needed for function
library(sf)
library(ebirdst)

# eBird Status & Trends access key
# An access key is required to download eBird Status & Trends data.
#
# Request or view your key here:
#    https://ebird.org/st/request
#
# Save your access key as a text file someone on your computer


#set your ebird access key. You should only have to run this once. 
set_ebirdst_access_key("************") # REPLACE WITH YOUR 12 DIGIT ALPHANUMERIC EBIRD ACCESS KEY

#Create folder to contain spatial data used for the analysis, 
#including species distribution models and polygons for proposed NAWCA projects
#This will create the following three nested folders within the root directory where
#JV_abundance_estimates.Rproj was saved and loaded from.
dir.create("data/spatial/polygons", recursive = TRUE) #only need to run this one. 

#If using DUC waterfowl density models (PHJV only), you will need to download those rasters manually and save them to the
#appropriate folder. First run:
dir.create("data/spatial/ducRasters")

#Visit the online PHJV Information Repository and navigate to:
#DSS v3 Waterfowl Species Models and download the 7 .tif files and save them to "data/spatial/ducRasters"


#Save all polygons in "data/spatial/polygons". Polygons can be saved in any format accepted by
#the sf package in R (e.g. .shp, .gpkg, .gbd)
#Then load them into R as a list of individual polygons
#ITS IMPORTANT THAT ALL THE POLYGONS ARE INPUT INTO A NAMED LIST
#OUTPUT FROM THE POPULATION ESTIMATION FUNCTION WILL SUMMARIZE RESULTS
#BASED ON THE NAMES YOU APPLY TO YOUR LIST OF POLYGONS

# a) Example using .gpkg file
sf::st_layers("data/spatial/polygons/ATWR.gpkg") #list layers within the gpkg file
sf::st_layers("data/spatial/polygons/SAND.gpkg") #list layers within the gpkg file
sand <- sf::st_read(dsn = "data/spatial/polygons/SAND.gpkg", layer = "SAND_boundary")
atwr <- sf::st_read(dsn = "data/spatial/polygons/ATWR.gpkg", layer = "ATWR_boundary")
polys <- list("SAND" = sand, "ATWR" = atwr) #creates a named list of polygons

# b) Example using a single shapefile that contains polygons for multiple sites
polysf <- sf::st_read(dsn = "data/spatial/polygons/examplePolys.shp") 
polys <- split(polysf, seq_len(nrow(polysf))) #split individual polygon into a list
names(polys) <- polysf$NAME_E #assign names to the list of polygons based on the "NAME_E" column from the attribute table

# c) Example using multiple shapefiles
poly1 <- sf::st_read("data/spatial/polygons/examplePoly1.shp")
poly2 <- sf::st_read("data/spatial/polygons/examplePoly2.shp")
polys <- list("poly1" = poly1, "poly2" = poly2)


#Create a list of species using common names
# a) Examples using a custom list
species <- c("Mallard", "Baird's Sparrow", "Common Yellowthroat")

# b) Example pulling species from NAWCA Priority Species list (table included in R project)
priority_spp <- read.csv("LookupData/nawca_acad_species_match.csv")
priority_spp$NAWCA_species #View list of all priority species
species <- c("Western Grebe", "Nelson's Sparrow", "Yellow-breasted Chat") #copy and paste desired species into this list

#Note that you could provide the entire priority species list to the popEst function, but this would
#take a VERY long time to run and its is NOT recommended.
#Its best to include smaller groups of species in separate runs of the function
#######################################################################################################


#3. Implement population estimation function for the list of polygons and species
#Note this can take a while to run, especially if your polygon and/or species list is long
source("functions/Function_PopEsts.R")
pop_ests <- popEsts(species, polys)
#######################################################################################################

