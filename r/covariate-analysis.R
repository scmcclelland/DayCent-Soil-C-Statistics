# filename:     covariate-analysis.R    
# created:      30 April 2026
# last updated: 19 June 2026
# author:       Docker Clark

# description:  
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
# Filter to desired regions
#-------------------------------------------------------------------------------
# reset args[6] if desired
args[6] <- "United States of America"

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



#left join to avoid dropping rows (join by gridcell, rep, crop, and irr)
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

#remove rows w/ non-finite vals for annual SOC sequest
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
#Annual SOC sequest and total applied N
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
       y = expression("SOC Sequestration" ~ (Mg ~ ha^-1 ~ yr^-1)),
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
       y = expression("SOC Sequestration" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Total Applied N by Annual Delta SOC",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years"),
       fill = "Observations") +
  geom_text(data = appN_soc_r2, aes(x = x, y = y, label = paste0("R2 = ", round(r2*100, 2), "%")),
            color = "gray30", show.legend = F, vjust = 2) +
  facet_grid(cols = vars(crop), labeller = as_labeller(crop_names))

# TODO consider binning the initial N (mineral N by the quantiles of the log-transormed version)
# these bins could then be displayed as colors on the plots.
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
       y = expression("SOC Sequestration" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Total Applied N by Annual Delta SOC",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years"),
       color = "Quantile of\nLog-Initial N") +
  geom_text(data = appN_soc_r2, aes(x = x, y = y, label = paste0("R2 = ", round(r2*100, 2), "%")),
            color = "gray30", show.legend = F, vjust = 2) +
  facet_grid(cols = vars(crop), labeller = as_labeller(crop_names))
#getting a bit abstract here. this plot is less useful

#Annual SOC sequest and bulk density
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
       y = expression("SOC Sequestration" ~ (Mg ~ ha^-1 ~ yr^-1)),
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

#annual SOC sequestration and pH
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
       y = expression("SOC Sequestration" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Annual Delta SOC by Soil pH",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years"),
       color = "Crop") +
  geom_text(data = ph_r2, aes(x = x, y = y, label = paste0("R2 = ", round(r2*100, 2), "%")),
            color = "gray30", show.legend = F, vjust = 2) +
  facet_grid(cols = vars(crop), labeller = as_labeller(crop_names))

#annual SOC sequestration and CLAY content
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
       y = expression("SOC Sequestration" ~ (Mg ~ ha^-1 ~ yr^-1)),
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
       y = expression("SOC Sequestration" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Relative Soil Water Content by Annual Delta SOC",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years")) +
  theme_bw()

#-------------------------------------------------------------------------------
# Sub-global PDFs and CDFs
#-------------------------------------------------------------------------------
#ensure only one unique gridid is kept
dt_corn <- dt_corn[, .(an_d_s_SOC = mean(an_d_s_SOC)), by = gridid]
dt_soyb <- dt_soyb[, .(an_d_s_SOC = mean(an_d_s_SOC)), by = gridid]
dt_wheat <- dt_wheat[, .(an_d_s_SOC = mean(an_d_s_SOC)), by = gridid]

plot_options <- list(
  "Corn"        = dt_corn,
  "Soy"         = dt_soyb,
  "Wheat"       = dt_wheat,
  "Clay Content" = NULL,  # placeholder until created
  "Applied N"   = NULL)

{
choice <- menu(names(plot_options), 
               title = paste0("Filtered to: ", args[6], ". Specify next filter."))
if (choice == 0) {
  message("No selection made.")
} else if (is.null(plot_options[[choice]])) {
  message("Required data table not yet created.")
} else {
  dt_plot <- plot_options[[choice]]
  message("Filtered to: ", names(plot_options[choice]))
}
}
#a function which allows calculation of probabilities from PDFs and CDFs
ecdf_fn <- ecdf(dt_plot$an_d_s_SOC)

# PDF: Probability Density Function
{#specify a probability range to highlight if desired. otherwise skip
  #between x1 (lower bound) and x2 (upper bound)
  x1 <- quantile(dt_plot$an_d_s_SOC, probs = c(0.95))
  x2 <- quantile(dt_plot$an_d_s_SOC, probs = c(1))
  prob_range <- ecdf_fn(x2) - ecdf_fn(x1)
  #precompute density so we can shade a region
  dens <- density(dt_plot$an_d_s_SOC, adjust = 2)
  dens_dt <- data.table(x = dens$x, y = dens$y)
}

PDF.plot <- ggplot(dt_plot, aes(x = an_d_s_SOC)) +
  geom_density(fill = "#4e9d7e", color = "#2d6e56",
               alpha = 0.6, linewidth = 0.8,
               adjust = 2) +
  labs(title = paste("PDF: Soil Carbon Change Distribution", args[6], 
                     names(plot_options[choice]), sep = " | "),
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
    annotate("text", x = (x1 + x2)/2, y = max(dens_dt[x >= x1 & x <= x2]$y)+0.5,
             label = paste0("Upper 5th percentile:\n ", round(x1, 2), " < X < ", round(x2, 2)),
             size = 4, fontface = "bold") 
}
#call the plot
print(PDF.plot)

cdf_cols <- c("#2D6E56", "#4E9D7E", "#A07178", "#8A89C0", "#E8A020", "#77877B")
# CDF: Cumulative Density Function
#specify a threshold value to point out in an annotation
soc.thresh <- (0.5)
cdf.lines <- ecdf_fn(soc.thresh)

CDF.plot <- ggplot(dt_plot, aes(x = an_d_s_SOC)) +
  stat_ecdf(geom = "step", linewidth = 1.2, color = cdf_cols[1]) +
  geom_hline(yintercept = c(0.05, 0.5, 0.95),
             linetype = "dotted", color = "gray50", alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = paste(args[6], "CDF: Soil Carbon Change Distribution", 
                     names(plot_options[choice]), sep = " | "),
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
  scale_x_continuous(
    breaks = seq(0, 2.5, by = 0.5),
    limits = c(0, 2.5)) +
  annotate("text", x = 3, y = 0.95, label = paste("n =", format(nrow(dt_plot), big.mark = ",")),
           hjust = 1, size = 3.5, fontface = "bold")
if (exists("soc.thresh")) {
  CDF.plot <- CDF.plot +
    annotate("segment", x = soc.thresh, xend = soc.thresh,
             y = -Inf, yend = Inf,
             linetype = "dashed", color = cdf_cols[5], linewidth = 0.8) +
    annotate("point", x = soc.thresh, y = cdf.line, 
             color = cdf_cols[5], size = 2, shape = 21, fill = cdf_cols[1]) +
    annotate("text", x = (soc.thresh + 0.05), y = cdf.line,
             label = paste0("P(X ≤ ", soc.thresh, ") = ", 100*(round(cdf.line, 3)), "%"),
             hjust = -0.1, size = 3.5, fontface = "bold")
}
#call the plot
print(CDF.plot)


#-------------------------------------------------------------------------------
# Scenario analysis on CDF plots
#-------------------------------------------------------------------------------

#load in data for all desired scenarios 
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
  
  for (r in c("European Union", "Oceania")) {
    args[6] <- r
    message("Filtering to: ", r)
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
    
    #remove rows w/ non-finite vals for annual SOC sequest
    dt_filtered <- dt_filtered[!is.na(an_d_s_SOC), ]
    
    #filter to only necessary cols
    dt_filtered <- dt_filtered[ , .(gridid, crop, irr, rep, an_d_s_SOC)]
    
    #split dt by crop
    dt_corn <- dt_filtered[crop == "maiz", ]
    dt_wheat<- dt_filtered[crop == "wht",  ]
    dt_soyb <- dt_filtered[crop == "soyb", ]
    
    # build standardized names for each crop table
    dt_corn_name  <- paste0("dt_plot_", gsub(" ", "_", r), "_corn")
    dt_soyb_name  <- paste0("dt_plot_", gsub(" ", "_", r), "_soyb")
    dt_wheat_name <- paste0("dt_plot_", gsub(" ", "_", r), "_wheat")
    
    # rename an_d_s_SOC column to the current scenario code (use setnames for data.table)
    setnames(dt_corn,  "an_d_s_SOC", gsub("-", "_", args[3]))
    setnames(dt_soyb,  "an_d_s_SOC", gsub("-", "_", args[3]))
    setnames(dt_wheat, "an_d_s_SOC", gsub("-", "_", args[3]))
    
    if (exists(dt_corn_name)) {
      message("Region table already created. Adding SOC column for this scenario.")
      # subsequent iterations: join new scenario column onto existing table
      assign(dt_corn_name,
             get(dt_corn_name, envir = .GlobalEnv)[dt_corn, on = .(gridid, crop, irr, rep)],
             envir = .GlobalEnv)
      
      assign(dt_soyb_name,
             get(dt_soyb_name, envir = .GlobalEnv)[dt_soyb, on = .(gridid, crop, irr, rep)],
             envir = .GlobalEnv)
      
      assign(dt_wheat_name,
             get(dt_wheat_name, envir = .GlobalEnv)[dt_wheat, on = .(gridid, crop, irr, rep)],
             envir = .GlobalEnv)
      
    } else {
      message("Creating base tables for this region")
      # first iteration: create the tables fresh
      assign(dt_corn_name,  dt_corn,  envir = .GlobalEnv)
      assign(dt_soyb_name,  dt_soyb,  envir = .GlobalEnv)
      assign(dt_wheat_name, dt_wheat, envir = .GlobalEnv)
    }
  }
  Sys.sleep(3) #this helps the global environment catch up with the loop
}

#ecdf_fn <- ecdf(dt_plot$an_d_s_SOC)
#soc.thresh <- (0.5)
#cdf.lines <- ecdf_fn(soc.thresh)
cdf_cols <- c("#2D6E56", "#4E9D7E", "#A07178", "#8A89C0", "#77877B", "#E8A020")


#reset region
args[6] <- "European Union"
plot_prefix <- paste0("dt_plot_", gsub(" ", "_", args[6]))
dt_plot <- get(paste0(plot_prefix, "_corn"))





CDF.plot <- ggplot(dt_plot) +
  #render background cdf lines first so they appear below the main line
  stat_ecdf(aes(x = ccg), geom = "line", linewidth = 1.2, 
            color = cdf_cols[3], alpha = 1) +
  stat_ecdf(aes(x = ccg_res), geom = "line", linewidth = 1.2, 
            color = cdf_cols[3], alpha = 0.5, linetype = "dashed") +
  stat_ecdf(aes(x = ntill), geom = "line", linewidth = 1.2, 
            color = cdf_cols[2], alpha = 1) +
  stat_ecdf(aes(x = ntill_res), geom = "step", linewidth = 1.2, 
            color = cdf_cols[2], alpha = 0.5, linetype = "dashed") +
  #main cdf line is full stacked practices
  stat_ecdf(aes(x = ccg_ntill), geom = "line", linewidth = 1.2, color = cdf_cols[1]) +
  geom_hline(yintercept = c(0.05, 0.5, 0.95),
             linetype = "dotted", color = "gray50", alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = paste(args[6], "CDF: Soil Carbon Change Distribution", sep = " | "),
       subtitle = paste0("Timescale: ", yrs, " Years | ", "Crop: Corn UPDATE"),
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
  scale_x_continuous(
    breaks = seq(-0.5, 2.5, by = 0.5),
    limits = c(-0.25, 2.5)) +
  annotate("text", x = 2.5, y = 0.95, label = 
             paste("n =", format(length(unique(dt_plot$gridid)), big.mark = ",")),
           hjust = 1, size = 3.5, fontface = "bold")
if (exists("soc.thresh")) {
  CDF.plot <- CDF.plot +
    annotate("segment", x = soc.thresh, xend = soc.thresh,
             y = -Inf, yend = Inf,
             linetype = "dashed", color = cdf_cols[5], linewidth = 0.8) +
    annotate("point", x = soc.thresh, y = cdf.line, 
             color = cdf_cols[5], size = 2, shape = 21, fill = cdf_cols[1]) +
    annotate("text", x = (soc.thresh + 0.05), y = cdf.line,
             label = paste0("P(X ≤ ", soc.thresh, ") = ", 100*(round(cdf.line, 3)), "%"),
             hjust = -0.1, size = 3.5, fontface = "bold")
}
#call the plot
print(CDF.plot)

