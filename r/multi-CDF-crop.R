# filename:     multi-CDF-crop.R    
# created:      26 June 2026
# last updated: 16 July 2026
# author:       Docker Clark

# description: This script performs two main functions:
#                 1. Using multi-scenario data tables to create annotated CDF plots split by crop type.
#                 2. Reading-in or creating these tables by loading multiple daycent scenarios for a given region, cleaning and filtering before re-exporting these tables for later use.
#-------------------------------------------------------------------------------
# libraries 
#-------------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(ggridges)
library(stringr)
library(sf)
library(terra)
library(rstudioapi)

#-------------------------------------------------------------------------------
# directories and startup
#-------------------------------------------------------------------------------
#base path
b_path <- "/gpfs/projects/McClellandGroup/projects/woodwell/DayCent-Soil-C-Statistics/data"

args    <- commandArgs(trailingOnly = TRUE) 
args[1] <- "analysis-input"
args[2] <- "analysis-output"
args[3] <- "ccg-ntill"
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
  "North America" = c("United States of America", "Canada"),
  "Oceania"       = c('Australia', 'New Zealand'),
  "Europe"        = c('Albania', 'Andorra', 'Austria', 'Belarus', 'Belgium', 'Bosnia and Herzegovina',
                      'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
                      'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland',
                      'Italy', 'Kosovo', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg',
                      'Malta', 'Moldova', 'Monaco', 'Montenegro', 'Netherlands', 'North Macedonia',
                      'Norway', 'Poland', 'Portugal', 'Romania', 'Russian Federation', 'San Marino',
                      'Serbia', 'Slovak Republic', 'Slovenia', 'Spain', 'Sweden', 'Switzerland',
                      'Ukraine', 'United Kingdom', 'Vatican City',
                      'Faroe Islands (Den.)', 'Gibraltar (UK)', 'Guernsey (UK)', 'Isle of Man (UK)',
                      'Jersey (UK)', 'Svalbard (Nor.)', 'Greenland (Den.)'),
  "European Union" =c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus',
                      'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France',
                      'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy',
                      'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 'Netherlands',
                      'Poland', 'Portugal', 'Romania', 'Slovak Republic', 'Slovenia',
                      'Spain', 'Sweden'))

#-------------------------------------------------------------------------------
# Create or load multi-scenario tables for CDFs
#-------------------------------------------------------------------------------
#reset region and timescale if desired
args[6] <- "Oceania"
args[4] <- "20-yr"
yrs <- as.numeric(str_split(args[4], "-")[[1]][1])
#set output path for tables and viz
o_path <- paste(b_path, args[2], args[4], sep = "/")

#first check if the multi-scenario tables exist in the output dir
#if not, create them and save them there. Note that tables should be manually deleted if user suspects scenario data has changed.
if (file.exists(paste0(
  o_path, "/ccg_scenarios_", gsub(" ", "_", args[6]), "_", args[4], "_corn.csv"))) {
  message("Data tables found in output directory. Loading in as: 'dt_corn', 'dt_soyb', 'dt_wheat'")
  dt_corn <- fread(paste0(o_path, "/ccg_scenarios_", gsub(" ", "_", args[6]), "_", args[4], "_corn.csv"))
  dt_soyb <- fread(paste0(o_path, "/ccg_scenarios_", gsub(" ", "_", args[6]), "_", args[4], "_soyb.csv"))
  dt_wheat<- fread(paste0(o_path, "/ccg_scenarios_", gsub(" ", "_", args[6]), "_", args[4], "_wheat.csv"))
  
  
} else { #if the correct table does not exist, in the output directory, create it.
  message("Table not found. Creating...")
  for (s in c("ccg", "res", "ntill", "ccg-res", "ccg-ntill", "ntill-res")) {
    #reset args
    args[3] <- s
    #add a scenario "dt_scenario"
    time <- Sys.time() #track how long this takes to load
    load(paste0(b_path, "/", args[1], "/",      #base file path
                args[4], "/", args[5], "-",     #time scale & SOC delta
                args[3],".RData"))              #scenario code and extension
    duration <- round((Sys.time()-time), 3)
    message(paste0("Loaded ", scenario_labels[args[3]], " in ", duration, " seconds."))
    rm(time, duration) #delete after
    
    # join country data table to simulation data
    dt_scenario <- dt_scenario[WB_dt[,c('cell', 'WB_NAME', "x", "y")], on = .(gridid = cell)]
    # remove NAs
    dt_scenario <- dt_scenario[!is.na(crop)]
    setorder(dt_scenario, gridid)
    gc() #garbage collection
    
    #standardize summer and winter wheat to just wheat 
    dt_scenario[crop %in% c("swht", "wwht"), crop := "wht"]
    
    message("Filtering to ", args[6])
    if (args[6] == "Global") {
      countries <- unique(dt_scenario$WB_NAME)
    } else if (args[6] %in% names(regions)) {
      countries <- regions[[args[6]]]
    } else if (args[6] %in% dt_scenario$WB_NAME) {
      countries <- args[6]
    } else {
      message(args[6], " not found in filter function")
      countries <- NULL
    }
    #filter according to countries
    dt_filtered <- dt_scenario[WB_NAME %in% countries, ]
    
    #annualize SOC as a new column so either can be used
    yrs <- as.numeric(str_split(args[4], "-")[[1]][1])
    dt_filtered[, an_d_s_SOC := d_s_SOC / yrs]
    
    #remove rows w/ non-finite vals for annual SOC change
    dt_filtered <- dt_filtered[!is.na(an_d_s_SOC), ]
    
    #filter to only necessary cols
    dt_filtered <- dt_filtered[ , .(gridid, crop, irr, rep, an_d_s_SOC)]
    
    #split dt by crop
    dt_corn <- dt_filtered[crop == "maiz", ]
    dt_wheat<- dt_filtered[crop == "wht",  ]
    dt_soyb <- dt_filtered[crop == "soyb", ]
    
    # build standardized names for each crop table
    dt_corn_name  <- paste0("dt_plot_", gsub(" ", "_", args[6]), "_corn")
    dt_soyb_name  <- paste0("dt_plot_", gsub(" ", "_", args[6]), "_soyb")
    dt_wheat_name <- paste0("dt_plot_", gsub(" ", "_", args[6]), "_wheat")
    
    # rename an_d_s_SOC column to the current scenario code
    setnames(dt_corn,  "an_d_s_SOC", gsub("-", "_", args[3]))
    setnames(dt_soyb,  "an_d_s_SOC", gsub("-", "_", args[3]))
    setnames(dt_wheat, "an_d_s_SOC", gsub("-", "_", args[3]))
    
    if (exists(dt_corn_name)) {
      message("Adding SOC column for this scenario via safe merge.")
      
      #Assign the tables to the GE
      assign(dt_corn_name,
             merge(get(dt_corn_name, envir = .GlobalEnv), dt_corn, 
                   by = c("gridid", "crop", "irr", "rep"), all = TRUE),
             envir = .GlobalEnv)
      
      assign(dt_soyb_name,
             merge(get(dt_soyb_name, envir = .GlobalEnv), dt_soyb, 
                   by = c("gridid", "crop", "irr", "rep"), all = TRUE),
             envir = .GlobalEnv)
      
      assign(dt_wheat_name,
             merge(get(dt_wheat_name, envir = .GlobalEnv), dt_wheat, 
                   by = c("gridid", "crop", "irr", "rep"), all = TRUE),
             envir = .GlobalEnv)
      
    } else { 
      message("Creating base tables for this region")
      assign(dt_corn_name,  dt_corn,  envir = .GlobalEnv)
      assign(dt_soyb_name,  dt_soyb,  envir = .GlobalEnv)
      assign(dt_wheat_name, dt_wheat, envir = .GlobalEnv)
    }
    
    Sys.sleep(3) #this helps the global environment catch up with the loop
    
    #on last iteration, write the three crop tables
    if (s == "ntill-res") {
      for (crop in c("corn", "soyb", "wheat")) {
        table_name <- paste0("ccg_scenarios_", gsub(" ", "_", args[6]),
                             "_", args[4], "_", crop, ".csv")
        
        fwrite(x = get(paste("dt_plot", gsub(" ", "_", args[6]), crop, sep = "_")),
               file = paste0(o_path, "/", table_name))
      }
    }  
  }
  #overwrite the proper tables and remove clutter from the for loop
  dt_corn <- get(paste("dt_plot", gsub(" ", "_", args[6]), "corn", sep = "_"))
  dt_soyb <- get(paste("dt_plot", gsub(" ", "_", args[6]), "soyb", sep = "_"))
  dt_wheat<- get(paste("dt_plot", gsub(" ", "_", args[6]), "wheat", sep = "_"))
  
  rm(list = c(
    "dt_filtered", "dt_scenario", "crop", "dt_corn_name", "dt_soyb_name", "dt_wheat_name", "s", "table_name",
    paste("dt_plot", gsub(" ", "_", args[6]), "corn",  sep = "_"),
    paste("dt_plot", gsub(" ", "_", args[6]), "soyb",  sep = "_"),
    paste("dt_plot", gsub(" ", "_", args[6]), "wheat", sep = "_")
  ))
}

#-------------------------------------------------------------------------------
# Cumulative Density Function (CDF)
#-------------------------------------------------------------------------------
cdf_cols <- c("#2D6E56", "#4E9D7E", "#A07178", "#8A89C0", "#77877B", "#E8A020")

crop_names <- c("corn" = "Corn",
                "soyb" = "Soy",
                "wheat" = "Wheat")

#if desired, set an SOC threshold for annotation
soc.thresh <- 0.5 #in Mg / ha / yr

#loop creates a multi-CDF for each crop
for (crop in c("corn", "soyb", "wheat")) {
  #reset dt_plot according to crop
  dt_plot <- get(paste0("dt_", crop))
  
  #create cdf functions for annotation
  ecdf_ccg       <- ecdf(dt_plot$ccg)
  ecdf_res       <- ecdf(dt_plot$res)
  ecdf_ntill     <- ecdf(dt_plot$ntill)
  ecdf_ccg_res   <- ecdf(dt_plot$ccg_res)
  ecdf_ntill_res <- ecdf(dt_plot$ntill_res)
  ecdf_ccg_ntill <- ecdf(dt_plot$ccg_ntill)
  
  #helper function to format probability as a string for use in legend
  fmt_label <- function(name, p) {
    #formatted string includes common-language scenarios and formats probability as a rounded percent
    sprintf("%s  |  P(X\u2264%.2g) = %d%%", name, soc.thresh, round(100 * p))
  }
  
  #use the helper function to create strings for legend
  lab_ccg_ntill <- fmt_label("Full Stacked Practices",                ecdf_ccg_ntill(soc.thresh))
  lab_ntill     <- fmt_label("No-Till",                               ecdf_ntill(soc.thresh))
  lab_ntill_res <- fmt_label("No-Till + Residue Retention",           ecdf_ntill_res(soc.thresh))
  lab_ccg       <- fmt_label("Grass Cover Crop",                      ecdf_ccg(soc.thresh))
  lab_ccg_res   <- fmt_label("Grass Cover Crop + Residue Retention",  ecdf_ccg_res(soc.thresh))
  
  #order + styling lookup tables for the legend
  scenario_levels <- c(lab_ccg_ntill, lab_ntill, lab_ntill_res, lab_ccg, lab_ccg_res)
  
  scenario_colors <- setNames(
    c(cdf_cols[1], cdf_cols[2], cdf_cols[2], cdf_cols[3], cdf_cols[3]),
    scenario_levels)
  
  scenario_linetypes <- setNames(
    c("solid", "solid", "dashed", "solid", "dashed"),
    scenario_levels)
  
  #CDF plot code
  CDF.plot <- ggplot(dt_plot) +
    stat_ecdf(aes(x = ccg, color = lab_ccg, linetype = lab_ccg),
              geom = "line", linewidth = 1.2, alpha = 1) +
    stat_ecdf(aes(x = ccg_res, color = lab_ccg_res, linetype = lab_ccg_res),
              geom = "line", linewidth = 1.2, alpha = 0.5) +
    stat_ecdf(aes(x = ntill, color = lab_ntill, linetype = lab_ntill),
              geom = "line", linewidth = 1.2, alpha = 1) +
    stat_ecdf(aes(x = ntill_res, color = lab_ntill_res, linetype = lab_ntill_res),
              geom = "step", linewidth = 1.2, alpha = 0.5) +
    #render last so it appears on top of others
    stat_ecdf(aes(x = ccg_ntill, color = lab_ccg_ntill, linetype = lab_ccg_ntill),
              geom = "line", linewidth = 1.2) +
    #trace out important percentiles
    geom_hline(yintercept = c(0.05, 0.5, 0.95),
               linetype = "dotted", color = "gray50", alpha = 0.6) +
    scale_color_manual(name = "Scenario", values = scenario_colors, breaks = scenario_levels) +
    scale_linetype_manual(name = "Scenario", values = scenario_linetypes, breaks = scenario_levels) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = paste(args[6], "CDF: Soil Carbon Change Distribution", sep = " | "),
         subtitle = paste0("Timescale: ", yrs, " Years | ", "Crop: ", crop_names[crop]),
         x = expression("Soil Carbon Change (Mg C ha"^-1~"y"^-1*")"),
         y = "Cumulative Probability") +
    theme_bw() +
    theme(
      plot.title         = element_text(size = 13, face = "bold"),
      plot.subtitle      = element_text(size = 11),
      axis.text          = element_text(size = 10),
      axis.title         = element_text(size = 11),
      plot.background    = element_rect(fill = "white", color = NA),
      plot.margin        = margin(15, 15, 10, 10),
      legend.position    = c(0.98, 0.15),          # x,y in [0,1], relative to panel
      legend.justification = c(1, 0),
      legend.background  = element_rect(fill = scales::alpha("white", 0.7), color = "gray80"),
      legend.title       = element_text(size = 10, face = "bold"),
      legend.text        = element_text(size = 9),
      legend.key.width   = unit(1.2, "cm")) +
    scale_x_continuous(breaks = seq(-0.5, 2.5, by = 0.5)) +
    coord_cartesian(xlim = c(-0.25, 2.5)) +
    annotate("text", x = 2.5, y = 0.95, label = 
               paste("n =", format(length(unique(dt_plot$gridid)), big.mark = ",")),
             hjust = 1, size = 3.5, fontface = "bold") +
    
    #threshold line only. per-scenario probabilities now shown in the legend text
    annotate("segment", x = soc.thresh, xend = soc.thresh, y = -Inf, yend = Inf,
             linetype = "dashed", color = cdf_cols[6], linewidth = 0.8)
  
  #render plot
  print(CDF.plot)
  #  ggsave(paste0("/gpfs/scratch/docclark/woodwell/DayCent-Soil-C-Statistics/output", 
  #                "/multi-CDF_", gsub(" ", "_", args[6]), "_", crop_names[crop], ".png"),
  #         width = 8.5, height = 5, units = "in", dpi = 300)
}