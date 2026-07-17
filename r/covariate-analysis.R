# filename:     covariate-analysis.R    
# created:      30 April 2026
# last updated: 17 July 2026
# author:       Docker Clark

# description: This script joins daycent scenario tables with covariate tables, filters, and creates exploratory plots to identify additional influences on delta SOC.
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
#dir = dirname(getActiveDocumentContext()$path)
#dir = str_split(dir, '/r')
#dir = dir[[1]][1]
#setwd(dir)

b_path = paste(dir, 'data', sep = '/')
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

#add a scenario "dt_scenario"
{
  time <- Sys.time() #track how long this takes to load
  load(paste0(b_path, "/", args[1], "/",      #base file path
              args[4], "/", args[5], "-",     #time scale & SOC delta
              args[3],".RData"))              #scenario code and extension
  duration <- round((Sys.time()-time), 3)
  message(paste0("Loaded ", scenario_labels[args[3]], " in ", duration, " seconds."))
  rm(time, duration) #delete after
}
# join country data table to simulation data
dt_scenario <- dt_scenario[WB_dt[,c('cell', 'WB_NAME', "x", "y")], on = .(gridid = cell)]
# remove NAs
dt_scenario <- dt_scenario[!is.na(crop)]
setorder(dt_scenario, gridid)
gc() #garbage collection

#-------------------------------------------------------------------------------
# specify regions for filtering
#-------------------------------------------------------------------------------
regions <- list(
  "Global"        = unique(WB_dt[ , WB_NAME]),
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
                      'Spain', 'Sweden'),
  "USA"           = c("United States of America"))

#-------------------------------------------------------------------------------
# Filter to desired regions
#-------------------------------------------------------------------------------
# reset args[6] if desired
args[6] <- "USA"

# create and append regional lookup table
region_dt <- rbindlist(
  lapply(names(regions), function(r) data.table(region = r, WB_NAME = regions[[r]])))

#allow.cartesian allows for rows to be added when a WB_NAME belongs two region groups
# ex. France now has duplicate rows labeled "Global" and "European Union"
dt_scenario <- merge(dt_scenario, region_dt, by = "WB_NAME", allow.cartesian = TRUE)

#filter to correct region
dt_filtered <- dt_scenario[region == args[6], ]

#annualize SOC as a new column so either can be used
yrs <- as.numeric(str_split(args[4], "-")[[1]][1])
dt_filtered[, an_d_s_SOC := d_s_SOC / yrs]

#collapse monte carlo reps into the mean value for each gridcell-crop-irr
dt_filtered <- dt_filtered[, lapply(.SD, mean), .SDcols = c("d_s_SOC", "an_d_s_SOC"),
                           by = .(gridid, crop, irr, WB_NAME)]
#-------------------------------------------------------------------------------
# load and join data tables
#-------------------------------------------------------------------------------
#Covariate tables
load(paste0(b_path, "/", args[1], "/", "input_table_by_gridid_crop_irr.RData"))
dt_covars <- fread(paste0(b_path, "/", args[1], "/", "input_site_data.csv"))

#filter for variables of interest
main_table <- main_table[, .(gridid, crop, irr, x, y, fertN.amt, orgN.amt, orgCN.ratio,
                             res.rtrn.amt, frac_NH4, frac_NO3, frac_Urea)]
dt_covars <- dt_covars[ , .(gridid, crop, irr, ELEV, MINERL_sum_, NITRAT_sum_,
                            RWCF_sum_, SLBLKD, SLCLAY, SLPH, SLSAND)]


#left join to avoid dropping rows (join by gridcell, crop, and irr)
dt_filtered <- main_table[dt_filtered, on = .(gridid, crop, irr)]

#dt_covars does not split wht into summer and winter
#standardize summer and winter wheat to just wheat before joining
dt_filtered[crop %in% c("swht", "wwht"), crop := "wht"]
dt_filtered <- dt_covars[dt_filtered, on = .(gridid, crop, irr)]

#calculate total applied N
dt_filtered[, appN.total := fertN.amt + orgN.amt]

#mineral N: Check for absurdly high or below-zero values
range(dt_filtered$MINERL_sum_, na.rm = T)
dt_filtered[MINERL_sum_ > 10000, MINERL_sum_ := NA]
dt_filtered[MINERL_sum_ < 0 , MINERL_sum_ := NA]
range(dt_filtered$MINERL_sum_, na.rm = T)

# the mineral N appears to be log-normal. transform 
plot(density(dt_filtered[, MINERL_sum_], na.rm = T))
dt_filtered[, log_minerl := log(MINERL_sum_)]
plot(density(dt_filtered[, log_minerl], na.rm = T))

#remove rows w/ non-finite vals for annual SOC Change
dt_filtered <- dt_filtered[!is.na(an_d_s_SOC), ]

#split dt by crop for later
dt_corn <- dt_filtered[crop == "maiz", ]
dt_wheat<- dt_filtered[crop == "wht",  ]
dt_soyb <- dt_filtered[crop == "soyb", ]
#-------------------------------------------------------------------------------
# Themes
#-------------------------------------------------------------------------------
crop_names <- c("maiz" = "Corn", "soyb" = "Soy", "wht" = "Wheat")
irr_labs <- c("0" = "Not Irrigated", "1" = "Irrigated")
cat_cols <- c("#FC8D62", "#8DA0CB", "#66C2A5", "#A6D854", "#FFD92F", "#E78AC3", "#E5C494","#B3B3B3")
cdf_cols <- c("#2D6E56", "#4E9D7E", "#A07178", "#8A89C0", "#77877B", "#E8A020")

#-------------------------------------------------------------------------------
# data exploration 
#-------------------------------------------------------------------------------
#Annual SOC change and total applied N
#pre compute the R2 to display as an annotation
appN_soc_r2 <- dt_filtered[ , .(x  = mean(range(appN.total)), y  = Inf, #top of plot area
                                r2 = summary(lm(appN.total ~ an_d_s_SOC, data = .SD))$r.squared), by = crop]
ggplot(dt_filtered, mapping = aes(x = appN.total, y = an_d_s_SOC)) +
  geom_point(shape = 16, alpha = 0.3, aes(color = as.factor(crop)), show.legend = F) +
  geom_smooth(data = dt_filtered[crop == "soyb", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_filtered[crop == "maiz", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_filtered[crop == "wht", ],  method = "lm", color = "gray30", se = F) +
  scale_color_manual(values = cat_cols) +
  theme_bw() +
  theme(legend.key = element_rect(fill = "white")) +
  labs(x = expression("Total Applied Nitrogen" ~ (g ~ m^-2 ~ yr^-1)),
       y = expression("SOC Change" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Total Applied N by Annual Delta SOC",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years"),
       color = "Crop") +
  geom_text(data = appN_soc_r2, aes(x = x, y = y, label = paste0("R2 = ", round(r2*100, 2), "%")),
            color = "gray30", show.legend = F, vjust = 2) +
  facet_grid(cols = vars(crop), labeller = as_labeller(crop_names))
#showing point density
ggplot(dt_filtered, mapping = aes(x = appN.total, y = an_d_s_SOC)) +
  geom_hex(bins = 50) +
  geom_smooth(data = dt_filtered[crop == "soyb", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_filtered[crop == "maiz", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_filtered[crop == "wht", ],  method = "lm", color = "gray30", se = F) +
  scale_color_manual(values = cat_cols) +
  scale_fill_viridis_c(option = "inferno") +
  theme_bw() +
  theme(legend.key = element_rect(fill = "white")) +
  labs(x = expression("Total Applied Nitrogen" ~ (g ~ m^-2 ~ yr^-1)),
       y = expression("SOC Change" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Total Applied N by Annual Delta SOC",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years"),
       fill = "Observations") +
  geom_text(data = appN_soc_r2, aes(x = x, y = y, label = paste0("R2 = ", round(r2*100, 2), "%")),
            color = "gray30", show.legend = F, vjust = 2) +
  facet_grid(cols = vars(crop), labeller = as_labeller(crop_names))

#adding coloring by the quantiles of log-transformed mineral N 
ggplot(dt_filtered[is.na(MINERL_sum_) == F, ], mapping = aes(y = an_d_s_SOC, x = appN.total)) +
  geom_point(shape = 16, alpha = 0.3, aes(color = log_minerl), show.legend = T) +
  geom_smooth(data = dt_filtered[crop == "soyb", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_filtered[crop == "maiz", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_filtered[crop == "wht", ],  method = "lm", color = "gray30", se = F) +
  scale_color_binned(breaks = quantile(dt_filtered$log_minerl, probs = c(0.25,0.5,0.75),
                                       na.rm = T),
                     palette = cat_cols) +
  theme_bw() +
  theme(legend.key = element_rect(fill = "white")) +
  labs(x = expression("Total Applied Nitrogen" ~ (g ~ m^-2 ~ yr^-1)),
       y = expression("SOC Change" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Total Applied N by Annual Delta SOC",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years"),
       color = "Quantile of\nLog-Initial N") +
  geom_text(data = appN_soc_r2, aes(x = x, y = y, label = paste0("R2 = ", round(r2*100, 2), "%")),
            color = "gray30", show.legend = F, vjust = 2) +
  facet_grid(cols = vars(crop), labeller = as_labeller(crop_names))
#getting a bit abstract here. this plot is less useful

#Annual SOC change and bulk density
#precompute R-sq
bd_r2 <- dt_filtered[ , .(x  = mean(range(SLBLKD, na.rm = T)), y  = Inf, #top of plot area
                          r2 = summary(lm(SLBLKD ~ an_d_s_SOC, data = .SD))$r.squared), by = crop]
ggplot(dt_filtered[is.na(SLBLKD) == F, ], mapping = aes(y = an_d_s_SOC, x = SLBLKD)) +
  geom_point(shape = 16, alpha = 0.3, aes(color = crop), show.legend = F) +
  geom_smooth(data = dt_filtered[crop == "soyb", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_filtered[crop == "maiz", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_filtered[crop == "wht", ],  method = "lm", color = "gray30", se = F) +
  scale_color_manual(values = cat_cols) +
  theme_bw() +
  theme(legend.key = element_rect(fill = "white")) +
  labs(x = expression("Soil Bulk Density" ~ (g ~ cm^-3)),
       y = expression("SOC Change" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Annual Delta SOC by Bulk Density",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years"),
       color = "Crop") +
  geom_text(data = bd_r2, aes(x = x, y = y, label = paste0("R2 = ", round(r2*100, 2), "%")),
            color = "gray30", show.legend = F, vjust = 2) +
  facet_grid(cols = vars(crop), labeller = as_labeller(crop_names))

# comparing initial N with total applied N
ggplot(dt_filtered, aes(x = log_minerl, y = appN.total)) +
  geom_point(shape = 16, alpha = 0.3, aes(color = crop), show.legend = F) + 
  geom_smooth(method = "lm", color = "gray30") +
  scale_color_manual(values = cat_cols) +
  theme_bw() +
  labs(x = "Log-Transformed Mineral N Stock",
       y = expression("Total Applied N" ~ (g ~ m^-2 ~ yr^-1)),
       title = "Applied and Initial N Stocks",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years"),) +
  facet_grid(cols = vars(crop), labeller = as_labeller(crop_names))

#annual SOC change and pH
#precompute R2
ph_r2 <- dt_filtered[ , .(x  = mean(range(SLPH)), y  = Inf, #top of plot area
                          r2 = summary(lm(SLPH ~ an_d_s_SOC, data = .SD))$r.squared), by = crop]
ggplot(dt_filtered, aes(y = an_d_s_SOC, x = SLPH)) +
  geom_point(shape = 16, alpha = 0.3, aes(color = crop), show.legend = F) +
  geom_smooth(data = dt_filtered[crop == "soyb", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_filtered[crop == "maiz", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_filtered[crop == "wht", ],  method = "lm", color = "gray30", se = F) +
  scale_color_manual(values = cat_cols) +
  theme_bw() +
  theme(legend.key = element_rect(fill = "white")) +
  labs(x = expression("Soil pH"),
       y = expression("SOC Change" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Annual Delta SOC by Soil pH",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years"),
       color = "Crop") +
  geom_text(data = ph_r2, aes(x = x, y = y, label = paste0("R2 = ", round(r2*100, 2), "%")),
            color = "gray30", show.legend = F, vjust = 2) +
  facet_grid(cols = vars(crop), labeller = as_labeller(crop_names))

#annual SOC Change and CLAY content
#precompute R2
clay_r2 <- dt_filtered[ , .(x  = mean(range(SLCLAY)), y  = Inf, #top of plot area
                            r2 = summary(lm(SLCLAY ~ an_d_s_SOC, data = .SD))$r.squared), by = crop]
ggplot(dt_filtered, aes(y = an_d_s_SOC, x = SLCLAY)) +
  geom_point(shape = 16, alpha = 0.3, aes(color = crop), show.legend = F) +
  geom_smooth(data = dt_filtered[crop == "soyb", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_filtered[crop == "maiz", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_filtered[crop == "wht", ],  method = "lm", color = "gray30", se = F) +
  scale_color_manual(values = cat_cols) +
  theme_bw() +
  theme(legend.key = element_rect(fill = "white")) +
  labs(x = expression("Soil Clay Fraction"),
       y = expression("SOC Change" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Annual Delta SOC by Soil Clay Fraction",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years"),
       color = "Crop") +
  geom_text(data = clay_r2, aes(x = x, y = y, label = paste0("R2 = ", round(r2*100, 2), "%")),
            color = "gray30", show.legend = F, vjust = 2) +
  facet_grid(cols = vars(crop), labeller = as_labeller(crop_names))
# these and the ph covariate plots have some slight discreteness leading to bands of points. Consider a jitter

#relative water content
ggplot(dt_filtered, mapping = aes(x = RWCF_sum_, y = an_d_s_SOC)) +
  geom_point(aes(color = as.factor(irr)), alpha = 0.3) +
  scale_color_manual(values = cat_cols, labels = c("maiz" = "Corn", "soyb" = "Soy", "wht" = "Wheat")) +
  facet_grid(cols = vars(crop), labeller = as_labeller(crop_names)) +
  labs(color = "Irrigation", x = "Soil Water Content", 
       y = expression("SOC Change" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Relative Soil Water Content by Annual Delta SOC",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years")) +
  theme_bw()