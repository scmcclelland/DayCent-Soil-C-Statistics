# file name:    gridcell-level-means-table.R
# created:      23 September 2026
# last updated: 23 September 2026
# author:       Docker Clark

# description: This script creates and outputs a table pixel-level annual delta SOC, essentially collapsing model
# uncertainty (which was initially represented in monte carlo iterations "rep")
#-------------------------------------------------------------------------------
# libraries 
#-------------------------------------------------------------------------------

library(data.table)
library(rstudioapi)
library(stringr)
library(sf)
library(terra)

#-------------------------------------------------------------------------------
# directories and startup
#-------------------------------------------------------------------------------
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)

#command line args 
args     = commandArgs(trailingOnly = TRUE) 
#these can be updated for different scenarios
args[1] <- "data/analysis-input"
args[2] <- "data/analysis-output"
args[3] <- "ccg-ntill"
args[4] <- "20-yr"
args[5] <- "delta-cumulative-SOC"
args[6] <- "Global"

#check if there's enough info to get a filepath
if (isFALSE(length(args) == 6)) stop( 'Needs 6 command-line argument (scenario selection, timeframe, data path,
                                      input/output, data file header).' )

#set input data directory
in_dir <- paste(dir, args[1], sep = '/')
#set output data directory
o_dir <- paste(dir, args[2], sep = '/')
shp_p <- paste(in_dir, "shp", sep = "/")

#for later labeling
scenario_labels <- c(
  "conv"      = "Conventional / BAU",
  "res"       = "Full Residue Retention",
  "ntill"     = "No-Tillage",
  "ccg"       = "Grass Cover Crop",
  "ccl"       = "Legume Cover Crop",
  "ntill-res" = "No-Tillage & Full Residue Retention",
  "ccg-res"   = "Grass Cover Crop & Full Residue Retention",
  "ccl-res"   = "Legume Cover Crop & Full Residue Retention",
  "ccg-ntill" = "Grass Cover Crop, No-Tillage & Full Residue Retention",
  "ccl-ntill" = "Legume Cover Crop, No-Tillage & Full Residue Retention")

#-------------------------------------------------------------------------------
# Add World Bank Names
#-------------------------------------------------------------------------------
r_shp <- st_read(paste(shp_p, 'WB_countries_Admin0_10m.shp', sep = '/'))
r <- rast(paste(in_dir, 'msw-cropland-rf-ir-area.tif', sep = '/'))
r <- r[[1]]

create_WB_cty <- function(shp_f, rst) {
  shp_dt <- as.data.table(st_drop_geometry(shp_f))
  country_sf <- st_transform(shp_f, crs(rst))
  country_r <- terra::rasterize(
    x = vect(country_sf),
    y = rst,
    field = "OBJECTID",
    touches = TRUE
  )
  country_dt <- as.data.table(as.data.frame(country_r, cells = TRUE, xy = TRUE))
  shp_names <- data.table(WB_NAME = shp_dt$WB_NAME,
                          ID = shp_dt$OBJECTID)
  country_dt <- country_dt[shp_names, on = .(OBJECTID = ID)]
  return(country_dt)
}

WB_dt <- create_WB_cty(r_shp, r)

#load in as dt_scenario
load(paste0(in_dir, "/", args[4], "/",       #base file path & time scale
            args[5], "-", args[3], ".RData")) #SOC delta & scenario code

#annualize SOC as a new column so either can be used
yrs <- as.numeric(str_split(args[4], "-")[[1]][1])
dt_scenario[, an_d_s_SOC := d_s_SOC / yrs]

#collapse into grid cell level means
pixel_means <- dt_scenario[ , .(mean_an_d_SOC = mean(an_d_s_SOC)), by = gridid]

#add World Bank names according to grid cell
pixel_means <- WB_dt[, c("cell", "WB_NAME")][pixel_means, on = .(cell = gridid)]

#rename cell to avoid confusion
setnames(pixel_means, "cell", "gridid")
setorder(pixel_means, gridid)
gc()

#Export
fwrite(pixel_means, paste0(o_dir, "/", args[4], "/", "grid-cell-means-", args[3], ".csv"))