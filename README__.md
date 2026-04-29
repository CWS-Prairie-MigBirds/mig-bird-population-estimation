# JV Abundance Estimates

## Relative to Absolute Bird Abundance Estimates

### Overview

This project converts **relative abundance estimates from eBird Status & Trends** into **absolute abundance estimates** within a user-defined conservation area.

The approach combines:

1. Spatial **relative abundance rasters** from eBird Status & Trends  
2. Continental or global **population estimates**  
3. Population estimates from the **Avian Conservation Assessment Database (ACAD)**  

The result is an estimate of the **number of individuals of a species expected within a conservation polygon**.

---

## Conceptual Framework

The method follows a **proportional scaling approach**.

- Relative abundance rasters from **eBird Status & Trends** represent the spatial distribution of individuals.
- The **sum of raster values across the modeled range** represents the full population distribution.
- The **proportion of abundance within a conservation area** is calculated.
- This proportion is then multiplied by a **continental or global population estimate from ACAD**.

---

## Formula

**Absolute Abundance in Area**

Absolute Abundance =
(Proportion of total relative abundance in area) × (Population estimate from ACAD)


---

## Applications

This framework is useful for:

- **Joint Venture science support**
- **Estimating bird populations within potential conservation areas**
- **Supporting conservation planning and prioritization**
