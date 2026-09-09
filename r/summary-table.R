# file name:    summary-table.R
# created:      24 July 2026
# last updated: 09 September 2026
# author:       Docker Clark

# description: This script creates and outputs a table of global and regional means for various scenarios.
# dependencies: World Bank country shapefile and cropland raster under data/analysis-input
#-------------------------------------------------------------------------------
# libraries 
#-------------------------------------------------------------------------------

library(data.table)
library(sf)
library(terra)
library(rstudioapi)
library(stringr)

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
args[3] <- "ccg"
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
# Create country lookup table
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

# Add desired regions
regions <- list(
  "Global"         = unique(WB_dt$WB_NAME),
  "Oceania"        = c('Australia', 'New Zealand'),
  "European Union" = c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus',
                       'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France',
                       'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy',
                       'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 'Netherlands',
                       'Poland', 'Portugal', 'Romania', 'Slovak Republic', 'Slovenia',
                       'Spain', 'Sweden'),
  "USA"            = c("United States of America"),
  "Brazil"         = c("Brazil"))

# create regional lookup table
region_dt <- rbindlist(
  lapply(names(regions), function(r) data.table(region = r, WB_NAME = regions[[r]])))

#-------------------------------------------------------------------------------
# Generate regional means
#-------------------------------------------------------------------------------
# this loop loads-in data for desired scenarios and appends world bank names and regions
# for later filtration. Note: resultant tables contain duplicate rows and must be filtered
# by region.
table_scenarios <- c("ccg", "res", "ntill", "ccg-res", "ntill-res", "ccg-ntill")
table_scenarios <- table_scenarios[table_scenarios %in% args[3]]
spatial_summaries <- list()
for (s in table_scenarios) {
  #load in as dt_scenario
  load(paste0(in_dir, "/", args[4], "/",       #base file path & time scale
              args[5], "-", s, ".RData")) #SOC delta & scenario code
  message(paste0("Loaded ", scenario_labels[s]))
  
  #annualize SOC as a new column so either can be used
  yrs <- as.numeric(str_split(args[4], "-")[[1]][1])
  dt_scenario[, an_d_s_SOC := d_s_SOC / yrs]

  # join country data table to simulation data
  dt_scenario <- WB_dt[, c('cell', 'WB_NAME', 'x', 'y')][dt_scenario, on = .(cell = gridid)]
  
  #rename cell to avoid confusion
  setnames(dt_scenario, "cell", "gridid")
  setorder(dt_scenario, gridid)
  gc() #garbage collection
  
  #allow.cartesian allows for rows to be added when a WB_NAME belongs two region groups
  # ex. France now has duplicate rows labeled "Global" and "European Union"
  dt_scenario <- merge(dt_scenario, region_dt, by = "WB_NAME", allow.cartesian = TRUE)

  #-----------------------------------------------------------------------------
  # Calculate global and regional means and spatial standard deviations
  #-----------------------------------------------------------------------------
  dt_grid_means <- dt_scenario[, .(
    mean_SOC = mean(an_d_s_SOC)
  ), by = .(region, gridid)]

  spatial_summaries[[s]] <- dt_grid_means[, .(
    scenario = scenario_labels[s],
    n_gridids = .N,
    Mean = mean(mean_SOC),
    SD = sd(mean_SOC)
  ), by = region]

  rm(dt_grid_means)
  
  message("Calculating means by region")
  dt_scenario <- dt_scenario[, .(
    Mean   = mean(an_d_s_SOC)), 
    by = .(region, rep)] #must include by = region
  
  #rename it according to s
  assign(paste0("dt_means_", gsub("-", "_", s)), dt_scenario)
  
  #remove dt_scenario on last iteration
  if (s == table_scenarios[length(table_scenarios)]) {rm(dt_scenario)}
}

#-------------------------------------------------------------------------------
# Build summary table
#-------------------------------------------------------------------------------
sum_table <- data.table()

sum_table <- rbindlist(lapply(table_scenarios, function(s) {
  dt <- get(paste0("dt_means_", gsub("-", "_", s)))
  dt[, .(
    Mean  = round(mean(Mean), 2),
    Lower = round(quantile(Mean, 0.025), 3),
    Upper = round(quantile(Mean, 0.975), 3)
  ), by = region][, scenario := scenario_labels[s]]
}))

# combine mean + interval into one formatted string per cell
sum_table[, cell_value := paste0(Mean, "\n(", Lower, " - ", Upper, ")")]

# reshape long -> wide: scenarios as rows, regions as columns
sum_table <- dcast(sum_table, scenario ~ region, value.var = "cell_value")

#output
fwrite(sum_table, paste0(o_dir, "/", args[4], "/",
                         "regional-means-table.csv"))

#-------------------------------------------------------------------------------
# Build global and regional spatial summary table
#-------------------------------------------------------------------------------
regional_spatial_table <- rbindlist(spatial_summaries)

fwrite(regional_spatial_table,
       paste0(o_dir, "/", args[4], "/", args[3],"-regional-spatial-summary.csv"))
