# filename:     geo-analysis-crop.R    
# created:      16 July 2026
# last updated: 17 July 2026
# author:       Docker Clark

# description: This script computes statistics and makes a visualizations for scenarios on a 10 or 20-yr timescale and at regional or national scales.
# It produces statistical visualizations split by crop monoculture.
#-------------------------------------------------------------------------------
# libraries 
#-------------------------------------------------------------------------------

library(data.table)
library(sf)
library(terra)
library(ggplot2)
library(ggridges)
library(stringr)

#-------------------------------------------------------------------------------
# directories and startup
#-------------------------------------------------------------------------------
#base data path
b_path <- "/gpfs/projects/McClellandGroup/projects/woodwell/DayCent-Soil-C-Statistics/data"

args    <- commandArgs(trailingOnly = TRUE) 
args[1] <- "analysis-input"
args[2] <- "analysis-output"
args[3] <- "ccg"
args[4] <- "20-yr"
args[5] <- "delta-cumulative-SOC"
args[6] <- "Europe"

#assuming shp_p means shapefile path
shp_p <- paste(b_path, args[1], "shp", sep = "/")

#add a scenario "dt_scenario"
load(paste0(b_path, "/", args[1], "/",      #base file path
            args[4], "/", args[5], "-",     #time scale & SOC delta
            args[3],".RData"))              #scenario code and extension

#annualize SOC as a new column so either can be used
yrs <- as.numeric(str_split(args[4], "-")[[1]][1])
dt_scenario[, an_d_s_SOC := d_s_SOC / yrs]

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
# ADD regions
#-------------------------------------------------------------------------------
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
# join country data table to simulation data
dt_scenario <- dt_scenario[WB_dt[,c('cell', 'WB_NAME', "x", "y")], on = .(gridid = cell)]
# remove NAs
dt_scenario <- dt_scenario[!is.na(crop)]
setorder(dt_scenario, gridid)
gc() #garbage collection

#----------------------------------------------------------------
# Add IPCC Region Names
#----------------------------------------------------------------
# IPCC Region Names (AR6 & Roe et al. 2021)
# Africa and Middle East
AME   = c('Congo, Democratic Republic of', 'Nigeria', 'Tanzania', 'South Africa', 'Congo, Rep. of', 'Zambia',
          'Angola', 'Cameroon', 'Ethiopia', 'Mozambique', 'Iran, Islamic Republic of', 'Uganda',
          'Central African Republic', 'Gabon', 'Sudan', "Côte d'Ivoire", 'Kenya', 'Egypt, Arab Republic of',
          'Ghana', 'Zimbabwe', 'Mali', 'Namibia', 'South Sudan', 'Chad', 'Morocco', 'Botswana', 'Burkina Faso',
          'Niger', 'Guinea', 'Algeria', 'Liberia', 'Malawi', 'Senegal', 'Somalia', 'Saudi Arabia', 'Benin', 
          'Sierra Leone', 'Iraq', 'Rwanda', 'Eritrea', 'eSwatini', 'Benin', 'Burundi', 'Djibouti', 'Equatorial Guinea',
          'Madagascar', 'Mauritania', 'Tunisia', 'Syrian Arab Republic', 'Lebanon', 'Jordan', 'Libya', 'Israel', 
          'West Bank and Gaza', 'Kuwait', 'Oman', 'Qatar', 'United Arab Emirates', 'Yemen, Republic of', 'Cabo Verde',
          'Guinea-Bissau', 'Togo', 'Comoros', 'Mauritius', 'Lesotho', "Gambia, The", "Bahrain")
ADP   = c('China', 'Indonesia', 'India', 'Myanmar', 'Vietnam', 'Malaysia', 'Thailand', 'Pakistan', 'Papua New Guinea',
          'Philippines', 'Bangladesh', 'Cambodia', "Lao People's Democratic Republic", 'Mongolia', 'Korea, Republic of',
          'Afghanistan', 'Nepa', 'Sri Lanka', "Korea, Democratic People's Republic of", 'Solomon Islands', 'Bhutan',
          'Timor-Leste', 'Fiji', 'Nepal', 'Hong Kong (SAR, China)', 'Brunei Darussalam', 'Samoa', 'Vanuatu', 'Tonga')
DEV   = c('United States of America', 'Canada', 'Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Czech Republic', 'Denmark',
          'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy', 'Latvia', 'Lithuania', 'Luxembourg',
          'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovak Republic', 'Slovenia','Spain', 'Sweden', 'United Kingdom', 'Australia', 'Ukraine',
          'Japan', 'Turkey', 'New Zealand', 'Norway', 'Iceland', 'Greenland (Den.)', 'Faroe Islands (Den.)', 'Switzerland', 'Saint-Pierre-et-Miquelon (Fr.)',
          'Cyprus', 'Puerto Rico (US)', 'American Samoa (US)', 'Saint Helena, Ascension and Tristan da Cunha (UK)', 'New Caledonia (Fr.)',
          'French Southern and Antarctic Lands (Fr.)', 'Falkland Islands (UK)/Islas Malvinas', 'South Georgia and South Sandwich Islands (UK)')
EEWCA = c('Russian Federation', 'Kazakhstan', 'Belarus', 'Uzbekistan', 'Turkmenistan', 'Kyrgyz Republic', 'Azerbaijan',
          'Moldova', 'Tajikistan', 'Armenia', 'Serbia', 'Bosnia and Herzegovina', 'Georgia', 'Montenegro', 'Kosovo', 'Albania',
          'North Macedonia')
LAC   = c('Brazil', 'Colombia', 'Mexico', 'Argentina', 'Bolivia', 'Peru', 'Venezuela', 'Paraguay', 'Ecuador', 'Chile', 'Guyana', 'Suriname',
          'Cuba', 'Uruguay', 'Honduras', 'Nicaragua', 'Guatemala', 'Guyana', 'Costa Rica', 'Panama', 'Dominican Republic', 'El Salvador', 'Belize',
          'Bahamas, The', 'Haiti', 'Turks and Caicos Islands (UK)', 'Jamaica', 'Venezuela, Republica Bolivariana de', 'Trinidad and Tobago')

#creating IPCC names
dt_scenario[WB_NAME %in% AME, IPCC_NAME   := 'AME']
dt_scenario[WB_NAME %in% ADP, IPCC_NAME   := 'ADP']
dt_scenario[WB_NAME %in% DEV, IPCC_NAME   := 'DEV']
dt_scenario[WB_NAME %in% EEWCA, IPCC_NAME := 'EEWCA']
dt_scenario[WB_NAME %in% LAC, IPCC_NAME   := 'LAC']

# check if missing but the groups above should capture everything
if (nrow(dt_scenario[is.na(IPCC_NAME), .(WB_NAME, IPCC_NAME)]) > 0) {
  #which countries are not captured
  missing <- dt_scenario[is.na(IPCC_NAME), unique(WB_NAME)]
  message("Some countries not captured. Missing: ", paste(missing, sep = ", "))
} else {
  message("All countries captured.")
}

#-------------------------------------------------------------------------------
# Additional Desired Regions
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
# Filter to desired regions
#-------------------------------------------------------------------------------
# reset args[6] if desired
args[6] <- "Global"

#filter to correct region
dt_filtered <- dt_scenario[region == args[6], ]

#-------------------------------------------------------------------------------
# Statistics
#-------------------------------------------------------------------------------
#summary stats
dt_stats <- dt_filtered[, .(
  Min = min(an_d_s_SOC),
  P25 = quantile(an_d_s_SOC, probs = 0.25),
  Median = quantile(an_d_s_SOC, probs = 0.50),
  Mean = mean(an_d_s_SOC),
  P75 = quantile(an_d_s_SOC, probs = 0.75),
  Max = max(an_d_s_SOC)), 
  by = .(rep)] 

#-------------------------------------------------------------------------------
# Shared Themes
#-------------------------------------------------------------------------------
#color schemes (continuous)
linecols <- viridis::viridis(6)

#color schemes (categorical)
cat_cols <- c("#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3")

#-------------------------------------------------------------------------------
# PDF: Probability Density Function
#-------------------------------------------------------------------------------
#split dataset by crop
dt_corn <- dt_filtered[crop == "maiz",]
dt_soyb <- dt_filtered[crop == "soyb",]
dt_wheat <- dt_filtered[crop %in% c("swht", "wwht"), ]

#crop names for plot labels
crop_names <- c("corn" = "Corn",
                "soyb" = "Soy",
                "wheat" = "Wheat")

# This loop creates 3 (crop-filtered) PDFs for the chosen region/timescale/scenario. 
for (crop in c("corn", "soyb", "wheat")) {
  dt_plot <- get(paste0("dt_", crop))
  
  #a function which allows calculation of probabilities from PDFs and CDFs
  ecdf_fn <- ecdf(dt_plot$an_d_s_SOC)
  #specify a probability range to highlight if desired. otherwise skip
  #between x1 (lower bound) and x2 (upper bound)
  x1 <- quantile(dt_plot$an_d_s_SOC, probs = c(0.95))
  x2 <- quantile(dt_plot$an_d_s_SOC, probs = c(1))
  prob_range <- ecdf_fn(x2) - ecdf_fn(x1)
  #precompute density so we can shade a region
  dens <- density(dt_plot$an_d_s_SOC, adjust = 2)
  dens_dt <- data.table(x = dens$x, y = dens$y)
  
  
  PDF.plot <- ggplot(dt_plot, aes(x = an_d_s_SOC)) +
    geom_density(fill = "#4e9d7e", color = "#2d6e56",
                 alpha = 0.6, linewidth = 0.8,
                 adjust = 2) +
    labs(title = paste("PDF: Soil Carbon Change Distribution", args[6], 
                       crop_names[crop], sep = " | "),
         subtitle = paste("Scenario:", scenario_labels[args[3]], "| Timescale:", yrs, "Years"),
         x = expression("Soil Carbon Change (Mg C ha"^-1~"y"^-1*")"),
         y = "Probability Density") +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(size = 13, face = "bold"),
      plot.subtitle      = element_text(size = 11),
      axis.text          = element_text(size = 10),
      axis.title         = element_text(size = 11),
      axis.line          = element_line(color = "grey70"),
      plot.background    = element_rect(fill = "white", color = NA),
      plot.margin        = margin(15, 15, 10, 10)) +
    scale_x_continuous(
      breaks = seq(-0.5, 2.5, by = 0.5),
      limits = c(-0.5, 2.5))
  if (exists("dens")) {
    PDF.plot <- PDF.plot +
      geom_ribbon(data = dens_dt[x >= x1 & x <= x2],
                  aes(x = x, ymin = 0, ymax = y),
                  fill = "#e8a020", alpha = 0.6) +
      annotate("text", x = 1.75, y = 0.5,
               label = paste0("Upper 5th percentile:\n ", round(x1, 2), " < X < ", round(x2, 2)),
               size = 4, fontface = "bold") 
  }
  #call the plot
  print(PDF.plot)
}

#-------------------------------------------------------------------------------
# Singular CDF: Cumulative Density Function
#-------------------------------------------------------------------------------
for (crop in c("corn", "soyb", "wheat")) {
  dt_plot <- get(paste0("dt_", crop))
  
  #a function which allows calculation of probabilities from PDFs and CDFs
  ecdf_fn <- ecdf(dt_plot$an_d_s_SOC)
  #specify a threshold value to point out in an annotation
  soc.thresh <- (0.5)
  cdf.line <- ecdf_fn(soc.thresh)
  
  CDF.plot <- ggplot(dt_plot, aes(x = an_d_s_SOC)) +
    stat_ecdf(geom = "step", linewidth = 1.2, color = "#2d6e56") +
    geom_hline(yintercept = c(0.05, 0.5, 0.95),
               linetype = "dotted", color = "gray50", alpha = 0.6) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = paste(args[6], "CDF: Soil Carbon Change Distribution", crop_names[crop], sep = " | "),
         subtitle = paste("Scenario:", scenario_labels[args[3]], "| Timescale:", yrs, "Years"),
         x = expression("Soil Carbon Change (Mg C ha"^-1~"y"^-1*")"),
         y = "Cumulative Probability") +
    theme_bw() +
    theme(
      plot.title      = element_text(size = 13, face = "bold"),
      plot.subtitle   = element_text(size = 11),
      axis.text       = element_text(size = 10),
      axis.title      = element_text(size = 11),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(15, 15, 10, 10)) +
    scale_x_continuous(breaks = seq(0, 3, by = 0.5)) +
    coord_cartesian(xlim = c(0, 2.5))
  if (exists("soc.thresh")) {
    CDF.plot <- CDF.plot +
      annotate("segment", x = soc.thresh, xend = soc.thresh,
               y = -Inf, yend = cdf.line,
               linetype = "dashed", color = "#e8a020", linewidth = 0.8) +
      annotate("segment", x = -Inf, xend = soc.thresh,
               y = cdf.line, yend = cdf.line,
               linetype = "dashed", color = "#e8a020", linewidth = 0.8) +
      annotate("point", x = soc.thresh, y = cdf.line, 
               color = "#2d6e56", size = 2, shape = 21, fill = "#e8a020") +
      annotate("text", x = (soc.thresh + 0.05), y = cdf.line,
               label = paste0("P(X ≤ ", soc.thresh, ") = ", 100*(round(cdf.line, 3)), "%"),
               hjust = -0.1, size = 3.5, fontface = "bold")
  }
  #call the plot
  print(CDF.plot)
}
