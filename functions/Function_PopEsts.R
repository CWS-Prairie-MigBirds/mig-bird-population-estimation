############################################################################
#Function title: Estimate bird species popultion size for given area(s)
#Input list of species and list of polygons (shapefiles) for which population estimates are needed. Function will output population 
#estiamtes using all available data sources for each species and polygon.
#Written by: Barry Robinson, barry.robinson@ec.gc.ca
#Date: March 25, 2026
############################################################################

#arguments:
#species = list of species for which population estimates are needed
#polys = list of shapefiles (sf objects) within which population estimates are needed

#Note that script 1_CreateLookupInfo.R is run first. This downloads and creates the appropriate look up tables and spatial layers to determine which data sets are available for given species and locations
#All lookup tables and spatial layers will be uploaded to the GitHub repo, so this step can be skipped by general users.

# ------------------------------------------------------------
# 1. Helper function to check dependencies
# ------------------------------------------------------------
check_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  
  if (length(missing) > 0) {
    stop(
      "The following packages are required but not installed: ",
      paste(missing, collapse = ", "),
      "\nInstall them with install.packages(c(", 
      paste0("'", missing, "'", collapse = ", "), "))"
    )
  }
}

# ------------------------------------------------------------
# 2. Estimate populations function
# ------------------------------------------------------------
popEsts <- function(species, polys) {
  
  #check for required packages
  required_pkgs <- c("sf", "dplyr", "BAMexploreR", "ebirdst", "terra", "osfr", "purrr", "tibble", "units", "tidyr")
  check_packages(required_pkgs)
  
  #load source tables for distribution models and population estimates
  sdmSources <- read.csv("LookupData/sdm_speciesList.csv")
  peSources <- read.csv("LookupData/popEst_speciesList.csv")
  
  #Function to determine if conservation polygons overlap with strata polygons
  polyOverlap <- function(pol) {
    poly_prj <- sf::st_transform(pol, terra::crs(peStrat))
    test <- sf::st_intersects(poly_prj, sf::st_union(peStrat), sparse = FALSE)
    return(as.logical(test))
  }
  
  #Function to create proportional relative abundance rasters for strata:
  #1. query regional strata that intersect with polygons. 
  #2. extract regional population estimates for those strata
  #3. Create proportional relative abundance surface for each strata
  prop_ebird_strata <- function(pol, sdm) {
    #1. query regional strata that intersect
    poly_prj <- sf::st_transform(pol, terra::crs(peStrat))
    #determine intersecting strata and remove slivers
    suppressWarnings(ints <- sf::st_intersection(peStrat, poly_prj))
    min_area <- units::set_units(3000000, "m^2") #using 3e6 m^2 as minimum area
    ints <- ints[sf::st_area(ints) > min_area, ] 
    strata <- peStrat |>
      dplyr::filter(stratum %in% ints$stratum)
    
    #2. extract population estimates for intersecting strata
    pepoly <- pe |>
      dplyr::filter(stratum %in% strata$stratum) |>
      dplyr::pull(pop_est)
    if(length(pepoly) > 0) {
      pepoly <- sum(pepoly)
    } else {
      pepoly <- 0
    }
    
    #3. Create proportional relative abudnace raster for each polygon, matched to the appropriate strata
    strata_v <- terra::vect(strata) |>
      terra::project(terra::crs(sdm))
    abd_strata <- terra::crop(sdm, strata_v, mask = T)
    
    total_strata <- terra::global(abd_strata, fun = "sum", na.rm = TRUE)
    prop_strata <- abd_strata / total_strata$sum
    
    return(list("pe" = pepoly, "prop_raster" = prop_strata))
  }
  
  #Function to estimate population size from eBird relative abundance surfaces
  pop_est_ebird <- function(pol, pe_sdm, popEstSource) {
    #extract population estimate and sdm from list
    pe <- pe_sdm[["pe"]]
    prop_strata <- pe_sdm[["prop_raster"]]
    
    poly_v <- terra::vect(pol) |>
      terra::project(terra::crs(prop_strata))
    
    poly_area <- terra::expanse(poly_v, unit = "km") #calculate area of polygon
    results <- terra::extract(prop_strata, poly_v, weights = T) |> #extract values of pixels that intersect with polygon
      dplyr::summarise(dplyr::across(-c(ID, weight),
                                     ~ sum(.x * weight, na.rm = T))) |> #sum pixel values weighted by proportion of pixel that occurs within the polygon
      tidyr::pivot_longer(cols = everything(),
                          names_to = "season",
                          values_to = "propPolySum") |>
      dplyr::mutate(pop_est = round(propPolySum * pe, -1)) |> #multiply weighted, summed proportions by total population size to get population estimate
      dplyr::mutate(density_sqkm = round(pop_est/poly_area, 3), #divide by area to get mean density
                    popEstSource = popEstSource) |>
      dplyr::select(season, popEstSource, pop_est, density_sqkm)
    return(results)
  }
  
  #Function to estimate population size from density model
  #pol = conservation polygon, sdm = species raster, fact = factor to transform units into individuals/pixel
  popEst_DensityModel <- function(pol, sdm, fact = 1, dataSource) {
    poly_v <- terra::vect(pol) |>
      terra::project(terra::crs(sdm))
    
    poly_area <- terra::expanse(poly_v, unit = "km") #calculate area of polygon
    pop_est <- terra::extract(sdm, poly_v, weights = T) |> #extract values of pixels that intersect with polygon
      dplyr::summarise(dplyr::across(-c(ID, weight),~ sum(.x * weight, na.rm = T))) |> #sum pixel values weighted by proportion of pixel that occurs within the polygon
      as.numeric() * fact #multiply by factor to convert density to individuals/pixel, if needed
    
    results <- tibble::tibble(pop_est = round(pop_est, -1),
                      density_sqkm = round(pop_est/poly_area, 3),
                      species = sp,
                      season = "breeding",
                      sdmSource = dataSource,
                      popEstSource = dataSource)
    return(results)
  }
  
  #loop through species and estimate population size for all available data sources
  results <- list()
  for (sp in species) {
    #check if any data sources for species
    sdm <- dplyr::filter(sdmSources, common_name == sp)
    
    if(nrow(sdm) == 0) {cat("No data available for", sp, "\n")} else {
      #identify sdms available
      cat('Estimating population size for "', sp, '" using the following data sources: \n', sep = "")
      
      #--------------------------------------
      #eBird workflow
      #--------------------------------------
      if(sdm$eBird == "Yes") {
        cat("\t eBird relative abundance surfaces \n")
        #create and set directory for eBird rasters
        ebirdFolder <- "data/spatial/eBirdRasters"
        dir.create(ebirdFolder, showWarnings = FALSE)
        wd <- getwd()
        Sys.setenv(EBIRDST_DATA_DIR = file.path(wd, ebirdFolder))
        
        #download eBird relative abundance surface if no already present
        spCode <- ebirdst::get_species(sp)
        if(!any(basename(list.dirs(ebirdFolder, recursive = TRUE)) == spCode)) {
          cat("\t Downloading eBird relative abundance surface \n")
          # Download seasonal max abundance data at 3 km
          suppressMessages(
            try({ebirdst::ebirdst_download_status(sp, pattern = "abundance_seasonal_mean_3km", download_occurrence = FALSE, dry_run = FALSE, force = TRUE)}, silent = TRUE)
          )
        }
        
        #load eBird relative abundance rasters
        abd <- ebirdst::load_raster(
          sp,
          product = "abundance",
          period = "seasonal",
          metric = "mean", # note that here we can use mean or max
          resolution = "3km")
        
        #Determine which large-scale population estimate to use. Prioritize regional estimates if possible (PIF or USFWS). Use ACAD global (non breeding) or US-Can (breeding) if regional are unavailable
        peSp <- dplyr::filter(peSources, common_name == sp)
        
        
        #Breeding Season Start
        ####################################################################
        #extract breeding season raster
        abd_breeding <- abd[["breeding"]]
        
        #Regional Pop Ests Start
        #################################################
        if(peSp$pif_reg == "Yes" | peSp$fws_reg == "Yes") {
          if(peSp$pif_reg == "Yes") {
            #load regional PIF estimates
            pe <- read.csv("LookupData/pif.csv") |>
              dplyr::filter(common_name == sp)
            
            #load strata for regional PIF estimates
            invisible(capture.output({
              peStrat <- sf::st_read(dsn = "LookupData/modelExtents.gpkg", layer = "pif_reg")
            }))
            
            #set population estimate source for export later
            psource <- "PIF regional"
          }
          
          if(peSp$fws_reg == "Yes") {
            #load regional USFWS estimates
            pe <- read.csv("LookupData/usfws.csv") |>
              dplyr::filter(common_name == sp)
            
            #load strata for regional USFWS estimates
            invisible(capture.output({
              peStrat <- sf::st_read(dsn = "LookupData/modelExtents.gpkg", layer = "usfws")
            }))
            
            #set population estimate source for export later
            psource <- "USFWS"
          }
          
          #test which, if any, polygons overlap with strata for population estiamtes
          polyTest <- purrr::map(polys, polyOverlap)
          
          #split polygons that overlap with regional strata and those that don't
          polys_reg <- polys[unlist(polyTest)]
          polys_canus <- polys[!unlist(polyTest)]
          
          if(length(polys_reg) > 0) {
            #extract population estimates and proportional relative abundance raster for each overlaping strata
            pe_prop_strata <- purrr::map(polys_reg, prop_ebird_strata, sdm = abd_breeding)
            
            #estimate population size for each strata
            pop_est_breeding_reg <- purrr::map2(polys_reg, pe_prop_strata, pop_est_ebird, popEstSource = psource)
            names(pop_est_breeding_reg) <- names(polys_reg)
          }
        }#Regional Pop Ests End
        ######################################################
        
        #ACAD Canada/US estimates Start
        ######################################################
        use_canus <- exists("polys_canus") && peSp$acad == "Yes" && length(polys_canus) > 0
        condition <- (peSp$acad == "Yes" && peSp$pif_reg == "No" && peSp$fws_reg == "No") ||
          use_canus
        
        if(condition) {
          polys_tmp <- if(use_canus) {polys_canus} else {polys}
          
          #load ACAD population estimates
          pe <- read.csv("LookupData/acad.csv") |>
            dplyr::filter(common_name == sp) |>
            dplyr::pull(acad_uscan)
          
          #load polygon for Canada/USA. Using pif_reg and manipulating to have only 1 stratum (Can/US)
          invisible(capture.output({
            canus <- sf::st_read(dsn = "LookupData/modelExtents.gpkg", layer = "pif_reg") |>
              dplyr::mutate(stratum = "canus") |>
              dplyr::group_by(stratum) |>
              dplyr::summarize(geom = sf::st_union(geom))
          }))
          
          #match projections, crop and mask eBird surface with Canada/USA polygon
          canus_v <- terra::vect(canus) |> #change to SpatVect
            terra::project(terra::crs(abd_breeding)) #project Canada/USA to match eBird before cropping
          abd_canus <- terra::crop(abd_breeding, canus_v, mask = T) #crop and mask eBird to Canada/USA
          
          #Calculate proportional relative abundance raster for CAN-US
          total_canus <- terra::global(abd_canus, fun = "sum", na.rm = TRUE)
          prop_canus <- abd_canus / total_canus$sum
          
          #Create list for input into pop_est function
          pe_prop_canus <- list("pe" = pe, "prop_raster" = prop_canus)
          
          #Estimate population size for each polygon
          pop_est_breeding_canus <- purrr::map(polys_tmp, pop_est_ebird, pe_sdm = pe_prop_canus, popEstSource = "ACAD Can/USA")
          names(pop_est_breeding_canus) <- names(polys_tmp)
        }#ACAD Canada/US estimates End
        
        #combine results with regional and Can-US estimates if both exists for a species (likely only waterfowl when one polygon is within USFWS strata and one is without)
        if (exists("pop_est_breeding_reg") && exists("pop_est_breeding_canus")) {
          pop_est_breeding <- c(pop_est_breeding_reg, pop_est_breeding_canus)
        } else if (exists("pop_est_breeding_reg")) {
          pop_est_breeding <- pop_est_breeding_reg
        } else if (exists("pop_est_breeding_canus")) {
          pop_est_breeding <- pop_est_breeding_canus
        }
        
        #remove unneeded polygon and raster lists to save memory
        rm(list = intersect(c("polys_reg", "polys_canus", "polys_tmp", "pe_prop_strata", "pe_prop_canus"), ls()))
        invisible(capture.output({gc()}))
        
        #Non-breeding season Start
        ###########################################################
        abd_nonbreed <- abd[[names(abd) != "breeding"]] 
        
        if(peSp$acad == "Yes") {
          #load ACAD population estimates
          pe <- read.csv("LookupData/acad.csv") |>
            dplyr::filter(common_name == sp) |>
            dplyr::pull(acad_global)
          
          #Calculate proportional relative abundance raster for global
          total_global <- terra::global(abd_nonbreed, fun = "sum", na.rm = T)
          prop_global <- abd_nonbreed / total_global$sum
          
          #create list for pop_est function
          pe_prop_global <- list("pe" = pe, "prop_raster" = prop_global)
          
          #Estimate non-breeding populations size for all polygons
          pop_est_nonbreed <- purrr::map(polys, pop_est_ebird, pe_sdm = pe_prop_global, popEstSource = "ACAD global")
          names(pop_est_nonbreed) <- names(polys)
        }
        
        #combine results from eBird for breeding and non-breeding season
        ebirdResults <- dplyr::bind_rows(
          c(pop_est_breeding, pop_est_nonbreed),
          .id = "polyID") |>
          dplyr::mutate(sdmSource = "eBird",
                 species = sp) |>
          dplyr::select(species, polyID, sdmSource, popEstSource, season, pop_est, density_sqkm) |>
          dplyr::arrange(polyID)
        rownames(ebirdResults) <- NULL
        
      } else {
        ebirdResults <- data.frame(species = NA,
                                   polyID = NA,
                                   sdmSource = NA,
                                   popEstSource = NA,
                                   season = NA,
                                   pop_est = NA,
                                   density_sqkm = NA)
      }#END OF eBIRD WORKFLOW
      
      #--------------------------------------
      #BAM workflow
      #--------------------------------------
      #create blank database
      bamResults <- data.frame(species = NA,
                               polyID = NA,
                               sdmSource = NA,
                               popEstSource = NA,
                               season = NA,
                               pop_est = NA,
                               density_sqkm = NA)
      
      if(sdm$BAMv4 == "Yes") {# | sdm$BAMv5 == "Yes") { #### Excluding V5 for now while there's a glitch in BAMexploreR
        #Determine which version is available for given species. Use V5 if available
        #ver <- if(sdm$BAMv5 == "Yes") {"v5"} else {"v4"}
        ver = "v4"
        
        #confirm that conservation polygons overlap with BAM AOI
        invisible(capture.output({
          peStrat <- sf::st_read(dsn = "LookupData/modelExtents.gpkg", layer = paste0("bam", ver))
        }))
        polyTest <- purrr::map(polys, polyOverlap)
        polys_tmp <- polys[unlist(polyTest)]
        
        if(length(polys_tmp) > 0) {
          cat("\t BAM density model \n")
          #download BAM raster for given species
          dir.create("data/spatial/bamRasters", showWarnings = FALSE)
          #extract 4-letter code from species table and download BAM raster layer
          spCode <- BAMexploreR::spp_tbl |>
            dplyr::filter(commonName == sp) |>
            dplyr::pull(speciesCode)
          
          #check if species raster is already downloaded, then download if needed
          if(!file.exists(file.path("data/spatial/bamRasters",paste0("pred-", spCode, "-CAN-Mean.tif")))) {
            cat("\t Downloading BAM density model...this takes time, please be patient \n")
            suppressMessages({
              abd <- BAMexploreR::bam_get_layer(spCode, ver, "data/spatial/bamRasters")
            })
          } else {
            abd <- list()
            abd[[spCode]] <- terra::rast(file.path("data/spatial/bamRasters",paste0("pred-", spCode, "-CAN-Mean.tif")))
          }
          
          #estimate population size for each polygon using existing function
          pop_est_bam <- purrr::map(polys_tmp, popEst_DensityModel,
                                    sdm = abd[[spCode]],
                                    fact = 100,
                                    dataSource = paste0("BAM", ver))
          names(pop_est_bam) <- names(polys_tmp)
          
          #combine BAM results
          bamResults <- dplyr::bind_rows(pop_est_bam, .id = "polyID") |>
            dplyr::select(species, polyID,sdmSource, popEstSource, season, pop_est, density_sqkm) |>
            dplyr::arrange(polyID)
        }
      }#END OF BAM WORKFLOW 
      
      #--------------------------------------
      #CGAM Workflow
      #--------------------------------------
      #create empty data
      cgamResults <- data.frame(species = NA,
                                polyID = NA,
                                sdmSource = NA,
                                popEstSource = NA,
                                season = NA,
                                pop_est = NA,
                                density_sqkm = NA)
      
      if(sdm$CGAMv1 == "Yes") {
        #confirm that conservation polygons overlap with BAM model extent
        invisible(capture.output({
          peStrat <- sf::st_read(dsn = "LookupData/modelExtents.gpkg", layer = "cgam")
        }))
        
        polyTest <- purrr::map(polys, polyOverlap)
        polys_tmp <- polys[unlist(polyTest)]
        
        if(length(polys_tmp) > 0) {
          cat("\t CGAM density model \n")
          
          #download CGAM raster for given species
          cgamDir <- "data/spatial/cgamRasters"
          dir.create(cgamDir, showWarnings = FALSE)
          spCode <- read.csv("LookupData/IBPSpeciesCodes.csv") |>
            dplyr::filter(COMMONNAME == sp) |>
            dplyr::pull(SPEC)
          
          file <- osfr::osf_retrieve_node("csugd") |>
            osfr::osf_ls_files(pattern = spCode)
          file_path <- file.path(cgamDir, file$name)
          
          if(!file.exists(file_path)) {
            cat("\t Downloading CGAM density model \n")
            osfr::osf_download(file, path = cgamDir)
          }
          abd <- terra::rast(file_path)
          
          #estimate population size
          pop_est_cgam <- purrr::map(polys_tmp, popEst_DensityModel,
                                     sdm = abd,
                                     fact = 1, #CGAM models are already in individuals/pixels
                                     dataSource = "CGAMv1")
          
          names(pop_est_cgam) <- names(polys_tmp)
          
          #combine CGAM results
          cgamResults <- dplyr::bind_rows(pop_est_cgam, .id = "polyID") |>
            dplyr::select(species, polyID,sdmSource, popEstSource, season, pop_est, density_sqkm) |>
            dplyr::arrange(polyID)
        }
        
      } #END of CGAM Workflow
      
      
      #--------------------------------------
      #DUC Workflow
      #--------------------------------------
      #create empty data
      ducResults <- data.frame(species = NA,
                               polyID = NA,
                               sdmSource = NA,
                               popEstSource = NA,
                               season = NA,
                               pop_est = NA,
                               density_sqkm = NA)
      if(sdm$DUC == "Yes") {
        invisible(capture.output({
          peStrat <- sf::st_read(dsn = "LookupData/modelExtents.gpkg", layer = "duc")
        }))
        
        polyTest <- purrr::map(polys, polyOverlap)
        polys_tmp <- polys[unlist(polyTest)]
        
        if(length(polys_tmp) > 0) {
          cat("\t DUC density model \n")
          
          #load DUC raster for given species
          spCode <- read.csv("LookupData/IBPSpeciesCodes.csv") |>
            dplyr::filter(COMMONNAME == sp) |>
            dplyr::pull(SPEC)
          
          ducDir <- "data/spatial/ducRasters"
          file <- list.files(ducDir, pattern = spCode)
          abd <- terra::rast(file.path(ducDir, file))
          
          #estimate population size
          pop_est_duc <- purrr::map(polys_tmp, popEst_DensityModel,
                                     sdm = abd,
                                     fact = 0.16, #DUC models have 0.4 x 0.4 km pixels = 0.16km^2
                                     dataSource = "DUC")
          
          names(pop_est_duc) <- names(polys_tmp)
          
          #combine DUC results
          ducResults <- dplyr::bind_rows(pop_est_duc, .id = "polyID") |>
            dplyr::select(species, polyID, sdmSource, popEstSource, season, pop_est, density_sqkm) |>
            dplyr::arrange(polyID)
        }
        
      } #END DUC Workflow
      
      #combine results across data sources
      results[[sp]] <- rbind(ebirdResults,
                             bamResults,
                             cgamResults,
                             ducResults) |>
        dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.))) |> #remove rows with all NAs
        dplyr::arrange(polyID, season, sdmSource)
      
      #clear environment and RAM before running next species
      keep <- c("polys","results", "sdmSources", "peSources", "polyOverlap", "prop_ebird_strata", "pop_est_ebird", "popEst_DensityModel")
      rm(list = setdiff(ls(), keep),
         envir = environment())
      invisible(capture.output({gc()}))
    }
    
  } #END OF SPECIES LOOP
  
  #combine results across species
  results <- do.call(rbind, results)
  rownames(results) <- NULL
  return(results)
} #END POP EST FUNCTION














