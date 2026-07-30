# filename:     geo-analysis-crop.R    
# created:      20 April 2026
# last updated: 30 July 2026
# author:       Docker Clark

# description: This script computes statistics and makes a visualizations for scenarios on a 10 or 20-yr timescale and at regional or national scales.
# For global paneled PDF visualization, stratified random sampling to conserve resources.
#-------------------------------------------------------------------------------
# libraries 
#-------------------------------------------------------------------------------

library(data.table)
library(sf)
library(terra)
library(ggplot2)
library(ggridges)
library(stringr)
library(cowplot)

#-------------------------------------------------------------------------------
# directories and startup
#-------------------------------------------------------------------------------
#dir = dirname(getActiveDocumentContext()$path)
#dir = str_split(dir, '/r')
#dir = dir[[1]][1]
#setwd(dir)

#base data path
b_path <- "/gpfs/projects/McClellandGroup/projects/woodwell/DayCent-Soil-C-Statistics/data"

args    <- commandArgs(trailingOnly = TRUE) 
args[1] <- "analysis-input"
args[2] <- "analysis-output"
args[3] <- "ccg"
args[4] <- "20-yr"
args[5] <- "delta-cumulative-SOC"
args[6] <- "Global"

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
dt_scenario <- WB_dt[, c('cell', 'WB_NAME', 'x', 'y')][dt_scenario, on = .(cell = gridid)]

#rename cell to avoid confusion
setnames(dt_scenario, "cell", "gridid")
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
  "USA"            = c("United States of America"),
  "Brazil"         = c("Brazil"))

#-------------------------------------------------------------------------------
# Shared Themes
#-------------------------------------------------------------------------------
#color schemes (continuous)
linecols <- viridis::viridis(6)

#color schemes (categorical)
cat_cols <- c("#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3")

#-------------------------------------------------------------------------------
# Populate data table for regional analysis
#-------------------------------------------------------------------------------
# create and append regional lookup table
region_dt <- rbindlist(
  lapply(names(regions), function(r) data.table(region = r, WB_NAME = regions[[r]])))

#allow.cartesian allows for rows to be added when a WB_NAME belongs two region groups
# ex. France now has duplicate rows labeled "Global" and "European Union"
dt_scenario <- merge(dt_scenario, region_dt, by = "WB_NAME", allow.cartesian = TRUE)

dt_means <- dt_scenario[, .(
  Mean   = mean(an_d_s_SOC)), 
  by = .(region, rep)]

#-------------------------------------------------------------------------------
# Filter to desired regions
#-------------------------------------------------------------------------------
# reset args[6] if desired
args[6] <- "USA"

#filter to correct region
dt_filtered <- dt_scenario[region == args[6], ]

#-------------------------------------------------------------------------------
# Statistics
#-------------------------------------------------------------------------------
#summary stats
dt_stats <- dt_filtered[, .(
#  Min = min(an_d_s_SOC),
#  P25 = quantile(an_d_s_SOC, probs = 0.25),
  Median = quantile(an_d_s_SOC, probs = 0.50),
#  Mean = mean(an_d_s_SOC),
  P75 = quantile(an_d_s_SOC, probs = 0.75),
  P90 = quantile(an_d_s_SOC, probs = 0.90)
#  Max = max(an_d_s_SOC)
  ), 
  by = .(rep)] 

#melt data to long for read-in to ggplot
dt_long <- melt(dt_stats,
                measure.vars = setdiff(colnames(dt_stats), "rep"),
                variable.name = "statistic",
                value.name = "SOC")

#-------------------------------------------------------------------------------
# Sub-Global Ridgeline plot
#-------------------------------------------------------------------------------
ggplot(dt_long, aes(x = SOC, y = statistic, fill = statistic)) +
  geom_density_ridges(alpha = 0.6, rel_min_height = 0.005,
                      color = "gray20", linewidth = 0.4,
                      bandwidth = 0.013) + #binwidth for smoothing
  scale_fill_manual(values = linecols) +
  scale_x_continuous(
    breaks = seq(0, 2, by = 0.5)) +
  coord_cartesian(xlim = c(0,2), clip = "off") +
  labs(x = bquote("Mg ha"^-1~"y"^-1~"SOC Change Over" ~ .(yrs) ~ "Years"),
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
ggsave(paste0("/gpfs/scratch/docclark/woodwell/DayCent-Soil-C-Statistics/output", 
              "/ridgeline_", args[3], "_", args[4], "_", args[6], ".png"),
       width = 8.5, height = 5, units = "in", dpi = 300)

#-------------------------------------------------------------------------------
# Multi-Region Histogram
#-------------------------------------------------------------------------------
region_levels <- sort(unique(dt_means$region))
fillcols <- setNames(cat_cols[1:length(region_levels)], region_levels)

ggplot(dt_means) +
  geom_histogram(aes(x = Mean, fill = region), alpha = 0.5, bins = 100) +
  scale_fill_manual(values = fillcols) +
  labs(x = expression("Mean SOC Change (Mg ha"^-1~"yr"^-1*")"),
       y = "Frequency",
       fill = "Region",
       title = "Distribution of Monte Carlo Means",
       subtitle = paste0(yrs, " Years | ", "Scenario: ", scenario_labels[args[3]])) +
  theme_classic() +
  theme(legend.position = c(0.85, 0.75),
        legend.background = element_rect(fill = "white", color = "grey90"))

#smoothed regional histogram
ggplot(dt_means) +
  geom_density(aes(x = Mean, fill = region, color = region), 
               alpha = 0.5, linewidth = 0.75) +
  scale_fill_manual(values = fillcols) +
  scale_color_manual(values = fillcols) +
  labs(x = expression("Mean SOC Change (Mg ha"^-1~"yr"^-1*")"),
       y = "Frequency",
       fill = "Region",
       color = "Region",
       title = "Distribution of Monte Carlo Means",
       subtitle = paste0(yrs, " Years | ", "Scenario: ", scenario_labels[args[3]])) +
  coord_cartesian(xlim = c(0.5, 1)) +
  theme_classic() +
  theme(legend.position = c(0.85, 0.75),
        legend.background = element_rect(fill = "white", color = "grey90"))

#ggsave(paste0("/gpfs/scratch/docclark/woodwell/DayCent-Soil-C-Statistics/output", 
#              "/regional_hist_", args[3], "_", args[4], ".png"),
#       width = 8.5, height = 5, units = "in", dpi = 300)

#-------------------------------------------------------------------------------
# PDF: Probability Density Function
#-------------------------------------------------------------------------------
#a function which allows calculation of probabilities from PDFs and CDFs
#ecdf_fn <- ecdf(dt_filtered$an_d_s_SOC)
##specify a probability range to highlight if desired. otherwise skip
##between x1 (lower bound) and x2 (upper bound)
#x1 <- quantile(dt_filtered$an_d_s_SOC, probs = c(0.95))
#x2 <- quantile(dt_filtered$an_d_s_SOC, probs = c(0.1))
#prob_range <- ecdf_fn(x2) - ecdf_fn(x1)
##precompute density so we can shade a region
#dens <- density(dt_filtered$an_d_s_SOC, adjust = 2)
#dens_dt <- data.table(x = dens$x, y = dens$y)

# no shading under the curve, instead annotate mean, med, P75, and P90 with lines
mux <-  mean(dt_filtered[, an_d_s_SOC])
medx <- quantile(dt_filtered$an_d_s_SOC, probs = c(0.5))
p75x <- quantile(dt_filtered$an_d_s_SOC, probs = c(0.75))
p90x <- quantile(dt_filtered$an_d_s_SOC, probs = c(0.9))

#create data table to allow for dynamic annotations
stat_dt <- data.table(
  stat  = factor(c("Mean", "Median", "P75", "P90"),
                 levels = c("Mean", "Median", "P75", "P90")),
  value = c(mux, medx, p75x, p90x))

#speed up render via stratified random sampling
if (args[6] == "Global") { 
  k <- 5000000 # k = total sample size 
  set.seed(07272026)
  #dt_filtered <- dt_filtered[, .SD[sample(.N, min(.N, k))], by = WB_NAME] 
  dt_filtered <- dt_filtered[sample(.N, min(.N, k))] #
}

#adapted for paneled display. 
PDF.plot <- ggplot(dt_filtered, aes(x = an_d_s_SOC)) +
  geom_density(fill = "#4e9d7e", color = "#2d6e56", 
               alpha = 0.6, linewidth = 0.8, adjust = 2) +
  geom_vline(data = stat_dt,
             aes(xintercept = value, color = stat),
             linewidth = 1, key_glyph = draw_key_path) +
  scale_color_manual(
    name = "Summary Stat",
    values = c("Mean"   = "#77877B",
               "Median" = "#8A89C0",
               "P75"    = "#A07178",
               "P90"    = "#e8a020")) +
  labs(#title = paste("PDF: Soil Carbon Change Distribution", args[6], sep = " | "),
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
  scale_x_continuous(breaks = seq(-0.5, 2.5, by = 0.5)) +
  coord_cartesian(xlim = c(-0.5, 2.5))
if (exists("dens")) {
  PDF.plot <- PDF.plot +
    geom_ribbon(data = dens_dt[x >= x1 & x <= x2],
                aes(x = x, ymin = 0, ymax = y),
                fill = "#e8a020", alpha = 0.6) +
    annotate("text", x = 1.75, y = 0.5,
             label = paste0("Upper 5th percentile:\n ", round(x1, 2), " < X < ", round(x2, 2)),
             size = 4, fontface = "bold") 
}
#call and assign the plot
print(PDF.plot)

#get legend for later
legend <- get_legend(PDF.plot)

#remove legend from plot panels 
PDF.plot <- PDF.plot + theme(legend.position = "none")
assign(paste0(gsub("-", "_", args[3]), "_PDF.plot"), PDF.plot)

#run after all three panels have been assigned above
stacked <- plot_grid(ccg_PDF.plot, ccg_res_PDF.plot, ccg_ntill_PDF.plot, ncol = 1, align = "v")
final_plot <- plot_grid(stacked, legend, ncol = 2, rel_widths = c(1, 0.2))
print(final_plot)

ggsave(paste0("/gpfs/scratch/docclark/woodwell/DayCent-Soil-C-Statistics/output", 
             "/regional_PDF_", args[6], "_", args[3], "_", args[4], ".png"),
      width = 8.5, height = 11, units = "in", dpi = 300)
#-------------------------------------------------------------------------------
# Singular CDF: Cumulative Density Function
#-------------------------------------------------------------------------------
#a function which allows calculation of probabilities from PDFs and CDFs
ecdf_fn <- ecdf(dt_filtered$an_d_s_SOC)
#specify a threshold value to point out in an annotation
soc.thresh <- (0.5)
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
