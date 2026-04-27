# filename:     geo-analysis.R    
# created:      20 April 2026
# last updated: 27 April 2026
# author:       Docker Clark

# description: 
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
args[3] <- 'ccg-ntill'
args[4] <- '20-yr'
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
  "ccl-ntill" = "Legume Cover Crop, No-Tillage & Full Residue Retention"
)
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
# ADD IPCC REGION NAMES
#----------------------------------------------------------------
# we may want to break out DEV (developed countries)
# the DEV grouping doesn't make sense environmentally
# for example, we might want 'North America' 'Europe' and 'Oceania'
# it might make sense to merge some of those into existing groups
# 
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
  message("All countries not captured. Missing: ", missing)
} else {
  message("All countries captured.")
}

#filtering for desired regions
if (args[6] == "Global") {
  message("Global analysis")
} else if (args[6] == "North America") {
  countries <- c("United States of America", "Mexico", "Canada")
} else if (args[6] == "Oceania") {
  countries <- c('Australia', 'New Zealand', 'Papua New Guinea', 'Solomon Islands',
                 'Fiji', 'Vanuatu', 'Samoa', 'Tonga', 'Kiribati', 'Micronesia, Fed. Sts.',
                 'Palau', 'Marshall Islands', 'Tuvalu', 'Nauru',
                 'New Caledonia (Fr.)', 'French Polynesia (Fr.)', 'Guam (US)',
                 'Northern Mariana Islands (US)', 'American Samoa (US)',
                 'Cook Islands (NZ)', 'Niue (NZ)', 'Wallis and Futuna (Fr.)')
} else if (args[6] == "Europe") {
  countries <- c('Albania', 'Andorra', 'Austria', 'Belarus', 'Belgium', 'Bosnia and Herzegovina',
                 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
                 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland',
                 'Italy', 'Kosovo', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg',
                 'Malta', 'Moldova', 'Monaco', 'Montenegro', 'Netherlands', 'North Macedonia',
                 'Norway', 'Poland', 'Portugal', 'Romania', 'Russian Federation', 'San Marino',
                 'Serbia', 'Slovak Republic', 'Slovenia', 'Spain', 'Sweden', 'Switzerland',
                 'Ukraine', 'United Kingdom', 'Vatican City',
                 'Faroe Islands (Den.)', 'Gibraltar (UK)', 'Guernsey (UK)', 'Isle of Man (UK)',
                 'Jersey (UK)', 'Svalbard (Nor.)', 'Greenland (Den.)')
} else if (args[6] %in% dt_scenario$WB_NAME){
  countries <- args[6] 
} else {
  countries <- NULL
  message(args[6], " not found in filter function")
}
dt_filtered <- dt_scenario[WB_NAME %in% countries, ]

#a function which allows calculation of probabilities from PDFs and CDFs
ecdf_fn <- ecdf(dt_filtered$an_d_s_SOC)
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

#melt data to long for read-in to ggplot
dt_long <- melt(dt_stats,
                measure.vars = setdiff(colnames(dt_stats), "rep"),
                variable.name = "statistic",
                value.name = "SOC")
#-------------------------------------------------------------------------------
# Visualizations
#-------------------------------------------------------------------------------
#Shared themes
#color scheme
linecols <- viridis::viridis(6)
#Ridgelines
ggplot(dt_long, aes(x = SOC, y = statistic, fill = statistic)) +
  geom_density_ridges(alpha = 0.6, rel_min_height = 0.01,
                      color = "gray20", linewidth = 0.4,
                      bandwidth = 0.035) + #binwidth for smoothing
  scale_fill_manual(values = linecols) +
  scale_x_continuous(
    breaks = seq(-1, 4, by = 0.5),
    limits = c(-1, 4)) +
  labs(x = bquote("Mg ha"^-1~"y"^-1~"SOC Sequestration Over" ~ .(yrs) ~ "Years"),
       y = NULL,
       title = "Distribution of Summary Statistics",
       subtitle = paste0(args[6], " | ", "Scenario - ", scenario_labels[args[3]]),
       caption = "Variation from 1,001 Monte Carlo draws") +
  theme_ridges(font_family = "sans") +
  theme(
    legend.position    = "none",
    plot.title         = element_text(size = 13, face = "bold"),
    plot.subtitle      = element_text(size = 11),
    plot.caption       = element_text(size = 8, color = "grey50"),
    axis.text          = element_text(size = 10),
    axis.title.x       = element_text(size = 11),
    panel.grid.major.x = element_line(color = "grey90"),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.margin        = margin(15, 15, 10, 10)
  )

#-------------------------------------------------------------------------------
# PDF: Probability Density Function
#-------------------------------------------------------------------------------
{#specify a probability range to highlight if desired. otherwise skip
  prob_range <- ecdf_fn(0.1) - ecdf_fn(0.05)
  #precompute density so we can shade a region
  dens <- density(dt_filtered$an_d_s_SOC, adjust = 2)
  dens_dt <- data.table(x = dens$x, y = dens$y)
  }

PDF.plot <- ggplot(dt_filtered, aes(x = an_d_s_SOC)) +
  geom_density(fill = "#4e9d7e", color = "#2d6e56",
               alpha = 0.6, linewidth = 0.8,
               adjust = 2) +
  labs(title = paste("PDF: Soil Carbon Change Distribution", args[6], sep = " | "),
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
    plot.margin        = margin(15, 15, 10, 10))
if (exists("dens")) {
  PDF.plot <- PDF.plot +
    geom_ribbon(data = dens_dt[x >= 0.25 & x <= 0.5],
                aes(x = x, ymin = 0, ymax = y),
                fill = "#e8a020", alpha = 0.6) +
    annotate("text", x = 0.375, y = max(dens_dt[x >= 0.25 & x <= 0.5]$y) / 2,
             label = paste0("P = ", round(prob_range, 3)),
             size = 4, fontface = "bold") 
}
#call the plot
PDF.plot

#-------------------------------------------------------------------------------
# CDF: Cumulative Density Function
#-------------------------------------------------------------------------------
#specify a threshold value to point out in an annotation
soc.thresh <- (0.65)
cdf.line <- ecdf_fn(soc.thresh)

CDF.plot <- ggplot(dt_filtered, aes(x = an_d_s_SOC)) +
  stat_ecdf(geom = "step", linewidth = 1.2, color = "#2d6e56") +
  geom_hline(yintercept = c(0.05, 0.5, 0.95),
             linetype = "dotted", color = "gray50", alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = paste(args[6], "CDF: Soil Carbon Change Distribution", sep = " | "),
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
    plot.margin     = margin(15, 15, 10, 10))
if (exists("soc.thresh")) {
  CDF.plot <- CDF.plot +
    annotate("segment", x = soc.thresh, xend = soc.thresh,
             y = 0, yend = cdf.line,
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
CDF.plot
#-------------------------------------------------------------------------------
# United States (contiguous) SOC map
#-------------------------------------------------------------------------------
dt_USA <- dt_scenario[WB_NAME  == "United States of America", ]
shp_USA <- r_shp[r_shp$WB_NAME == "United States of America", ]

ggplot(dt_USA, aes(x = x, y = y, fill = an_d_s_SOC)) +
  geom_raster() +
  scale_fill_distiller(palette = "PRGn", direction = 1,
                       name = "SOC Change\n(Mg C/ha/y)",
                       breaks = seq(min(dt_USA$an_d_s_SOC),
                                    max(dt_USA$an_d_s_SOC),
                                    length.out = 8)) +
  geom_sf(data = shp_USA, fill = NA, color = "black", linewidth = 0.5, inherit.aes = F) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
  labs(title = paste("Delta Cumulative SOC |", dt_USA$WB_NAME[1]),
       subtitle = paste0("Scenario: ", scenario_labels[args[3]], " | ", args[4]), 
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(legend.key.height = unit(2, "cm"))