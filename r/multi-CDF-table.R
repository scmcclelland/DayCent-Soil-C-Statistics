# filename:     multi-CDF-table.R    
# created:      16 July 2026
# last updated: 16 July 2026
# author:       Docker Clark

# description: This script creates, saves, and/or loads a large data.table object for the subsequent creation of a multi-CDF plot.
#-------------------------------------------------------------------------------
# libraries 
#-------------------------------------------------------------------------------

library(data.table)
library(stringr)
library(sf)
library(terra)
library(rstudioapi)

#-------------------------------------------------------------------------------
# directories and startup
#-------------------------------------------------------------------------------
#dir = dirname(getActiveDocumentContext()$path)
#dir = str_split(dir, '/r')
#dir = dir[[1]][1]
#setwd(dir)

#base path
b_path <- "/gpfs/projects/McClellandGroup/projects/woodwell/DayCent-Soil-C-Statistics/data"

args    <- commandArgs(trailingOnly = TRUE) 
args[1] <- "analysis-input"
args[2] <- "analysis-output"
args[3] <- "ccg"
args[4] <- "20-yr"
args[5] <- "delta-cumulative-SOC"
args[6] <- "Global"

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
# load in spatial data
#-------------------------------------------------------------------------------
# define shapefile path
shp_p <- paste(b_path, args[1], "shp", sep = "/")

# read in shape file #~/analysis-input/shp
r_shp   <- st_read(paste(shp_p, 'WB_countries_Admin0_10m.shp', sep = '/'))
# read in crop mask
r       <- rast(paste(b_path, args[1], 'msw-cropland-rf-ir-area.tif', sep = '/'))
# keep first layer terra::rasterize needs a single layer raster
r       <- r[[1]]

# create function
# inputs are the shape file and the raster (above)
create_WB_cty <- function(shp_f, rst) {
  # ORIGINAL WB NAME and OBJECTID
  shp_dt          <- setDT(as.data.table(st_drop_geometry(shp_f)))
  # MATCH resolution of simulation data, dimensions the same
  target.r    <- rst
  # CONFIRM SHP in same coord ref syst as target
  country.sf  <- st_transform(shp_f, crs(target.r))
  country_r   <- terra::rasterize(
    x       = vect(country.sf),
    y       = target.r,
    field   = "OBJECTID",
    touches = TRUE          # optional: include cells touched by polygons
  )
  # CREATE data.frame, merge
  new_shp_dt  <- as.data.frame(country_r, cells = TRUE, xy = TRUE)
  new_shp_dt  <- setDT(new_shp_dt) # data.table object
  # GET WB names to match to ID
  shp_names   <- data.table(WB_NAME   = shp_dt$WB_NAME,
                            ID        = shp_dt$OBJECTID)
  # JOIN with WB names
  new_shp_dt  <- new_shp_dt[shp_names, on = .(OBJECTID = ID)]  
  return(new_shp_dt)
}

# create country data table with function
WB_dt <- create_WB_cty(r_shp, r)

#-------------------------------------------------------------------------------
# specify regions for filtering
#-------------------------------------------------------------------------------
regions <- list(
  "Global"         = unique(WB_dt$WB_NAME),
  "Oceania"        = c('Australia', 'New Zealand'),
  "European Union" = c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus',
                       'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France',
                       'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy',
                       'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 'Netherlands',
                       'Poland', 'Portugal', 'Romania', 'Slovak Republic', 'Slovenia',
                       'Spain', 'Sweden'),
  "USA"            = c("United States of America"))

#-------------------------------------------------------------------------------
# Create or load multi-scenario tables for CDFs
#-------------------------------------------------------------------------------
#reset region and timescale if desired
yrs <- as.numeric(str_split(args[4], "-")[[1]][1])

#set output path for tables and viz
o_path <- paste(b_path, args[2], args[4], sep = "/")


input_file  <- paste0(b_path, "/", args[1], "/",      #base file path
                      args[4], "/", args[5], "-",     #time scale & SOC delta
                      args[3],".RData") 
output_file <- paste0(o_path, "/ccg_scenarios_", gsub(" ", "_", args[6]), "_", args[4], ".csv")

input_time  <- file.info(input_file)$mtime
output_time <- file.info(output_file)$mtime

#compare last mod date of input and output to know if update is needed
needs_rerun <- !file.exists(output_file) ||
  file.info(input_file)$mtime > file.info(output_file)$mtime

if (needs_rerun) {
  message("Data tables found in output directory. Loading in as: ''")
  message("Table not found or out of date. Creating...")
  time <- Sys.time() #track how long this takes 
  for (s in c("ccg", "res", "ntill", "ccg-res", "ccg-ntill", "ntill-res")) {
    #reset args
    args[3] <- s
    #add a scenario "dt_scenario"
    load(paste0(b_path, "/", args[1], "/",      #base file path
                args[4], "/", args[5], "-",     #time scale & SOC delta
                args[3],".RData"))              #scenario code and extension
    message(paste0("Loaded ", scenario_labels[args[3]]))
    
    # join country data table to simulation data
    dt_scenario <- dt_scenario[WB_dt[,c('cell', 'WB_NAME', "x", "y")], on = .(gridid = cell)]
    # remove NAs
    dt_scenario <- dt_scenario[!is.na(crop)]
    setorder(dt_scenario, gridid)
    gc() #garbage collection
    
    #standardize summer and winter wheat to just wheat 
    dt_scenario[crop %in% c("swht", "wwht"), crop := "wht"]
    
    #filter according to regions
    dt_filtered <- dt_scenario[WB_NAME %in% regions[[args[6]]], ]
    
    #annualize SOC as a new column so either can be used
    yrs <- as.numeric(str_split(args[4], "-")[[1]][1])
    dt_filtered[, an_d_s_SOC := d_s_SOC / yrs]
    
    #remove rows w/ non-finite vals for annual SOC change
    dt_filtered <- dt_filtered[!is.na(an_d_s_SOC), ]
    
    #filter to only necessary cols
    dt_filtered <- dt_filtered[ , .(gridid, crop, irr, rep, an_d_s_SOC)]
    
    #add scenario code as a factor variable
    dt_filtered[ , scenario := args[3]]
    
    # build standardized name for multi-cdf table
    dt_name <- paste0("dt_plot_", gsub(" ", "_", args[6]))
    
    if (exists(dt_name)) {
      message("Adding SOC column for this scenario via rbind.")
      assign(dt_name,
             rbind(get(dt_name), dt_filtered))
      
    } else { #first iteration: create table
      message("Creating base table for this region")
      assign(dt_name, dt_filtered, envir = .GlobalEnv)
    }
    
    #on last iteration, save the table
    if (s == "ntill-res") {
      table_name <- paste0("ccg_scenarios_", gsub(" ", "_", args[6]),
                           "_", args[4], ".csv")
      
      fwrite(x = get(dt_name),
             file = paste0(o_path, "/", table_name))
      
      # report how long the loop took
      duration <- round((Sys.time()-time), 3)
      
      message("Created ", dt_name, " and saved as: ", table_name, " in output dir.")
      message("Duration: ", duration, " min ")
      rm(time, duration) #delete after
    }  
  }
} else { #if the correct table doe exist, in the output directory, load it.
  message("Data are up to date. Loading in...")
  dt_plot <- fread(output_file)

}
