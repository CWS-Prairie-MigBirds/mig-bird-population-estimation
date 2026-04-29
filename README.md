# Estimate migratory bird populations from various data sources

## Introduction

This framework provides a standardized approach for estimating bird
abundance within tracts of land included in NAWCA Canada proposals when
data or published population estimates from specific tracts are not
available. This approach uses either direct or indirect methods to
estimate population sizes within tracts of land, depending on the
availability of species distribution models for a given area. If
available, the framework uses spatially explicit, species density models
to estimate population sizes directly. When species density models are
unavailable for a given species or region, the framework uses spatially
explicit relative abundance models from eBird in conjunction with either
global or regional population estimates to estimate population size
indirectly. This framework offers a repeatable method for generating
coarse-scale abundance estimates where finer-scale data from individual
tracts of land are unavailable. **Where available, applicants should
prioritize the use of site**‑**specific avian survey data or published
population estimates. Applicants need to carefully review and validate
estimates from this approach, ideally with input from someone with
knowledge of the site or species, to ensure they are reasonable and do
not overestimate expected benefits.**

## Limitations and caveats

### Interpretation of output values

Abundance values generated through this framework should be interpreted
as coarse estimates rather than precise counts. Given the assumptions
involved (e.g., proportional scaling from relative abundance and
reliance on broad-scale regional or global population estimates),
results are best reported and communicated as orders of magnitude (e.g.,
tens, hundreds, thousands) rather than exact numbers. 

### Need for local refinement and expert review

The presence and number of individuals of a species within a tract of
land should be reviewed and refined using additional sources of
information, including published literature, local monitoring data,
reports, and expert opinion.

Small-area estimates  
The relative abundance estimates used in this approach are derived from
either eBird relative abundance surfaces at a 3x3km resolution, or
species density models at either 0.8x0.8km or 1x1km. Abundance estimates
for areas smaller than 3x3km should be interpreted with caution, as they
could be more imprecise than estimates that integrate multiple pixels.

## Methods

### Species density models (breeding season only)

There are two sources of species-specific density models for birds in
Canada that predict breeding season density at the pixel level: the
[Boreal Avian Modelling Project](https://borealbirds.ca/) (BAM), which
spans all of Canada, and the [Central Grassland Avian Modelling
Project](https://osf.io/csugd/overview) (CGAM), which spans Bird
Conservation Region 11 (Prairie Potholes) in Canada and the entire
Central Great Plains of the USA. Since these models predict density for
each pixel, population size of land tract *t* is calculated by
estimating population size within each pixel *i* as the product of pixel
density *D<sub>i</sub>* and pixel area *A<sub>i</sub>*, and then summing
these values over all *n<sub>t</sub>* pixels within tract *t*:

$$N_{t} = \ \sum_{i = 1}^{n_{t}}{(D_{i}A_{i})}$$

### Species relative abundance models with large-scale population estimates (breeding and non-breeding seasons)

This approach combines eBird’s [spatially-explicit relative abundance
models](https://science.ebird.org/en/status-and-trends) (spatial
distribution) (Fink et al. 2025) with regional, continental, or global
populations estimates from three sources:

1.  The [Avian Conservation Assessment
    Database](https://pif.birdconservancy.org/avian-conservation-asessment-database-scores)
    (ACAD) provides both global and Canada-USA population estimates for
    a large number of species.

2.  The [Partners in Flight (PIF) Populations Estimates
    Database](https://pif.birdconservancy.org/population-estimate-database-scores/)
    provides breeding population estimates within Bird Conservation
    Region (BCR) × State/Province strata, primarily for landbird
    species.

3.  The [USFWS provides breeding population estimates for common
    waterfowl
    species](https://iris.fws.gov/APPS/ServCat/Reference/Profile/140698)
    within [Traditional Survey Area
    strata](https://www.fws.gov/media/waterfowl-breeding-population-and-habitat-survey-coverage-map)
    in various regions of Canada in the USA.

The general method is the same, regardless of which population estimates
are used. The eBird relative abundance surface is used to step the
larger-scale population estimates down to smaller tracts of land. For
breeding population estimates, regional PIF and USFWS estimates are
prioritized when available (primarily for landbirds and waterfowl,
respectively), otherwise ACAD Canada-USA estimates are used.
Non-breeding season estimates require the use of ACAD global population
estimates because species could be disbursed beyond Canada and the USA.

Applying regional breeding season estimates to relative abundance
surfaces for other seasons would inflate pixel-level proportional
population estimates because it assumes breeding individuals
redistribute only within the smaller region, ignoring global migration.
Constraining the full regional breeding total to a smaller seasonal
range artificially inflates pixel-level abundance, leading to
overestimated local densities. In contrast, using the global population
estimates assumes that individuals redistribute across their full global
range outside the breeding season.

#### Step 1 – Transform relative abundance to proportion of population per pixel

Using the eBird relative abundance raster for the appropriate season
(breeding, non-breeding, pre-breeding migration, post-breeding
migration), calculate the proportion of the total population in region
*r* (either PIF/USFWS regional strata, Canada-USA, or global) that
occurs in each pixel *i*:

$$p_{i|r} = \ \frac{w_{i|r}}{\sum_{k = 1}^{n_{r}}w_{k|r}}$$

where *w<sub>i\|r </sub>*is the relative abundance of pixel *i*, and *k*
indexes all *n<sub>r</sub>* pixels in region *r*. The region used to
estimate *p<sub>i\|r</sub>* will depend on the season for which
population size is being estimated and the source of data being used.

- For breeding season population estimates for landbirds, use the [PIF
  population estimates for the BCR × State/province
  stratum](https://pif.birdconservancy.org/population-estimate-database-scores/)
  where the land tract occurs.

- For [waterfowl breeding season
  estimates](https://iris.fws.gov/APPS/ServCat/Reference/Profile/140698),
  use USFWS population estimates within [the Traditional Survey Area
  stratum](https://www.fws.gov/media/waterfowl-breeding-population-and-habitat-survey-coverage-map)
  where the tract occurs.

- For breeding season estimates for all other species, use the US-Canada
  population estimates from
  [ACAD](https://pif.birdconservancy.org/avian-conservation-asessment-database-scores)

- For non-breeding season estimates for all species, use the global
  population estimates from
  [ACAD](https://pif.birdconservancy.org/avian-conservation-asessment-database-scores)

#### Step 2 – estimate population size for the land tract

The population size of each pixel *i* can now be estimated by
multiplying the proportion of the regional population within pixel *i*
(*p<sub>i\|r</sub>*) by the total population size of region *r*
(*N<sub>r</sub>*). Population size of tract *t* can then be estimated by
summing the pixel-level population estimates across all *n<sub>t</sub>*
pixels within the tract:

$$N_{t} = \ \sum_{k = 1}^{n_{t}}{(p_{i|r}N_{r})}$$

## Workflow of R script

R scripts have been developed to implement the methods described above
programmatically. This section described the basic workflow of the R
script

### Density model workflow

1.  From the density model raster, extract values of all pixels that
    intersect with polygon representing the proposed land tract

2.  If necessary, convert pixel density values from individual/unit area
    to individual/pixel by multiplying pixel values by pixel area in the
    appropriate units. For example, with a 1km<sup>2</sup> pixel:

$$\frac{2\ birds}{ha}\  \times \ \frac{100ha}{1{km}^{2}} = \frac{200\ birds}{pixel}$$

3.  Sum pixel values (individual/pixel) across all pixels that intersect
    with the tract of land (weighted by the proportional area of each
    pixel that occurs with the tract) to get the population estimate for
    the tract of land.

### Relative abundance model workflow – breeding season

1.  Crop and mask the eBird breeding-season relative abundance raster
    with polygon representing the region for which you have a
    large-scale population estimate (PIF/USFWS stratum or Canada-USA
    boundary).

2.  Sum relative abundance values across all pixels within the region

3.  Calculate the proportion of the regional population that occurs
    within each pixel by dividing each pixel’s relative abundance value
    by the sum of relative abundance values across the entire region.

4.  From the proportional population raster, extract the values of all
    pixels that intersect with the polygon representing the proposed
    tract of land.

5.  Sum pixel values (proportional population estimates) across all
    pixels that intersect with the tract of land (weighted by the
    proportional area of each pixel that occurs with the tract) to
    estimate the proportion of the regional population that occurs
    within the tract.

6.  Multiply the proportion of the regional population within the tract
    by the reginal population estimate to get the population estimate
    for the tract of land.

### Relative abundance model workflow – non-breeding season

1.  The global population estimate will be used for the non-breeding
    season, so it is not necessary to crop the eBird relative abundance
    raster.

2.  Sum relative abundance values across all pixels

3.  Calculate the proportion of the global population that occurs within
    each pixel by dividing each pixel’s relative abundance value by the
    sum of relative abundance values across all pixels.

4.  From the proportional population raster, extract the values of all
    pixels that intersect with the polygon representing the proposed
    tract of land.

5.  Sum pixel values (proportional population estimates) across all
    pixels that intersect with the tract of land (weighted by the
    proportional area of each pixel that occurs with the tract) to
    estimate the proportion of the global population that occurs within
    the tract.

6.  Multiply the proportion of the global population within the tract by
    the global population estimate to get the population estimate for
    the tract of land.

7.  Repeat this process with relative abundance rasters for the
    pre-breeding migration, post-breeding migration, and non-breeding
    seasons.
