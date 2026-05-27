# filename:     covariate-analysis.R    
# created:      30 April 2026
# last updated: 27 May 2026
# author:       Docker Clark

# description:  
#-------------------------------------------------------------------------------
# libraries 
#-------------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(ggridges)
library(stringr)

#-------------------------------------------------------------------------------
# directories and startup
#-------------------------------------------------------------------------------
#base path
b_path <- "/gpfs/projects/McClellandGroup/projects/woodwell/DayCent-Soil-C-Statistics/data"

args    <- commandArgs(trailingOnly = TRUE) 
args[1] <- "analysis-input"
args[2] <- "analysis-output"
args[3] <- "res"
args[4] <- "20-yr"
args[5] <- "delta-cumulative-SOC"

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
# load and join data tables
#-------------------------------------------------------------------------------
#Covariate tables
load(paste0(b_path, "/", args[1], "/", "input_table_by_gridid_crop_irr.RData"))
dt_covars <- fread(paste0(b_path, "/", args[1], "/", "input_site_data.csv"))

#filter for variables of interest
main_table <- main_table[, .(gridid, crop, irr, x, y, fertN.amt, orgN.amt, orgCN.ratio,
                             res.rtrn.amt, frac_NH4, frac_NO3, frac_Urea)]
dt_covars <- dt_covars[ , .(gridid, crop, irr, ELEV, MAXPH, MINERL_sum_, NITRAT_sum_,
                            RWCF_sum_, SLBLKD, SLCLAY, SLPH, SLSAND)]

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

#annualize SOC as a new column so either can be used
yrs <- as.numeric(str_split(args[4], "-")[[1]][1])
dt_scenario[, an_d_s_SOC := d_s_SOC / yrs]

#collapse monte carlo reps into the mean value for each gridcell-crop-irr
dt_scenario <- dt_scenario[, lapply(.SD, mean), .SDcols = c("d_s_SOC", "an_d_s_SOC"),
                             by = .(gridid, crop, irr)]

#left join to avoid dropping rows (join by gridcell, rep, crop, and irr)
dt_scenario <- main_table[dt_scenario, on = .(gridid, crop, irr)]

#dt_covars does not split wht into summer and winter
#standardize summer and winter wheat to just wheat before joining
dt_scenario[crop %in% c("swht", "wwht"), crop := "wht"]
dt_scenario <- dt_covars[dt_scenario, on = .(gridid, crop, irr)]

#calculate total applied N
dt_scenario[, appN.total := fertN.amt + orgN.amt]


#mineral N: Check for absurdly high or below-zero values
range(dt_scenario$MINERL_sum_, na.rm = T)
dt_scenario[MINERL_sum_ > 10000, MINERL_sum_ := NA]
dt_scenario[MINERL_sum_ < 0 , MINERL_sum_ := NA]
range(dt_scenario$MINERL_sum_, na.rm = T)

# the mineral N appears to be log-normal. transform 
plot(density(dt_scenario[, MINERL_sum_]))
dt_scenario[, log_minerl := log(MINERL_sum_)]
plot(density(dt_scenario[, log_minerl]))

#remove rows w/ non-finite vals for annual SOC sequest
dt_scenario <- dt_scenario[!is.na(an_d_s_SOC), ]

#split dt by crop for later
dt_corn <- dt_scenario[crop == "maiz", ]
dt_wheat<- dt_scenario[crop == "wht",  ]
dt_soyb <- dt_scenario[crop == "soyb", ]

#-------------------------------------------------------------------------------
# Themes
#-------------------------------------------------------------------------------
crop_names <- c("maiz" = "Corn", "soyb" = "Soy", "wht" = "Wheat")
irr_labs <- c("0" = "Not Irrigated", "1" = "Irrigated")
cat_cols <- c("#FC8D62", "#8DA0CB", "#66C2A5", "#A6D854", "#FFD92F", "#E78AC3", "#E5C494","#B3B3B3")

#-------------------------------------------------------------------------------
# data exploration 
#-------------------------------------------------------------------------------
#Annual SOC sequest and total applied N
#pre compute the R2 to display as an annotation
appN_soc_r2 <- dt_scenario[ , .(x  = mean(range(appN.total)), y  = Inf, #top of plot area
                                r2 = summary(lm(appN.total ~ an_d_s_SOC, data = .SD))$r.squared), by = crop]
ggplot(dt_scenario, mapping = aes(x = appN.total, y = an_d_s_SOC)) +
  geom_point(shape = 16, alpha = 0.3, aes(color = as.factor(crop)), show.legend = F) +
  geom_smooth(data = dt_scenario[crop == "soyb", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_scenario[crop == "maiz", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_scenario[crop == "wht", ],  method = "lm", color = "gray30", se = F) +
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

#Annual SOC sequest and bulk density
#precompute R-sq
bd_r2 <- dt_scenario[ , .(x  = mean(range(SLBLKD)), y  = Inf, #top of plot area
                                r2 = summary(lm(SLBLKD ~ an_d_s_SOC, data = .SD))$r.squared), by = crop]
ggplot(dt_scenario[is.na(SLBLKD) == F, ], mapping = aes(y = an_d_s_SOC, x = SLBLKD)) +
  geom_point(shape = 16, alpha = 0.3, aes(color = crop), show.legend = F) +
  geom_smooth(data = dt_scenario[crop == "soyb", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_scenario[crop == "maiz", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_scenario[crop == "wht", ],  method = "lm", color = "gray30", se = F) +
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


ggplot(dt_scenario, aes(y = an_d_s_SOC, x = log_minerl)) +
  geom_point(shape = 16, aes(color = crop)) +
  theme_bw() +
  facet_grid(cols = vars(crop))

dt_scenario <- dt_scenario[is.na(MINERL_sum_) == F, ]
ggplot(dt_scenario, aes(x = log_minerl, y = appN.total)) +
  geom_point() +
  geom_smooth()
plot(density(dt_scenario[, MINERL_sum_]))
plot(density(dt_scenario[, appN.total ]))
quantile(dt_scenario[, MINERL_sum_])
quantile(dt_scenario[, appN.total])
# TODO consider binning the initial N (mineral N by the quantiles of the log-transormed version)
# these bins could then be displayed as colors on the plots.
ggplot(dt_scenario, aes(x = log_minerl, y = appN.total)) +
  geom_point() + 
  geom_smooth(method = "lm")

#annual SOC sequestration and pH
#precompute R2
ph_r2 <- dt_scenario[ , .(x  = mean(range(SLPH)), y  = Inf, #top of plot area
                          r2 = summary(lm(SLPH ~ an_d_s_SOC, data = .SD))$r.squared), by = crop]
ggplot(dt_scenario, aes(y = an_d_s_SOC, x = SLPH)) +
  geom_point(shape = 16, alpha = 0.3, aes(color = crop), show.legend = F) +
  geom_smooth(data = dt_scenario[crop == "soyb", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_scenario[crop == "maiz", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_scenario[crop == "wht", ],  method = "lm", color = "gray30", se = F) +
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
clay_r2 <- dt_scenario[ , .(x  = mean(range(SLCLAY)), y  = Inf, #top of plot area
                            r2 = summary(lm(SLCLAY ~ an_d_s_SOC, data = .SD))$r.squared), by = crop]
ggplot(dt_scenario, aes(y = an_d_s_SOC, x = SLCLAY)) +
  geom_point(shape = 16, alpha = 0.3, aes(color = crop), show.legend = F) +
  geom_smooth(data = dt_scenario[crop == "soyb", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_scenario[crop == "maiz", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_scenario[crop == "wht", ],  method = "lm", color = "gray30", se = F) +
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

# Sand???
#precompute R2
sand_r2 <- dt_scenario[ , .(x  = mean(range(SLSAND)), y  = Inf, #top of plot area
                            r2 = summary(lm(SLSAND ~ an_d_s_SOC, data = .SD))$r.squared), by = crop]
ggplot(dt_scenario, aes(y = an_d_s_SOC, x = SLSAND)) +
  geom_point(shape = 16, alpha = 0.3, aes(color = crop), show.legend = F) +
  geom_smooth(data = dt_scenario[crop == "soyb", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_scenario[crop == "maiz", ], method = "lm", color = "gray30", se = F) +
  geom_smooth(data = dt_scenario[crop == "wht", ],  method = "lm", color = "gray30", se = F) +
  scale_color_manual(values = cat_cols) +
  theme_bw() +
  theme(legend.key = element_rect(fill = "white")) +
  labs(x = expression("Soil Sand Fraction"),
       y = expression("SOC Sequestration" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Annual Delta SOC by Soil Sand Fraction",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years"),
       color = "Crop") +
  geom_text(data = sand_r2, aes(x = x, y = y, label = paste0("R2 = ", round(r2*100, 2), "%")),
            color = "gray30", show.legend = F, vjust = 2) +
  facet_grid(cols = vars(crop), labeller = as_labeller(crop_names))
# consider a slight horizontal jitter

#relative water content
ggplot(dt_scenario, mapping = aes(x = RWCF_sum_, y = an_d_s_SOC)) +
  geom_point(aes(color = crop), alpha = 0.3) +
  scale_color_manual(values = cat_cols, labels = c("maiz" = "Corn", "soyb" = "Soy", "wht" = "Wheat")) +
  facet_grid(cols = vars(irr), labeller = as_labeller(irr_labs)) +
  labs(color = "Crop", x = "Soil Water Content", 
       y = expression("SOC Sequestration" ~ (Mg ~ ha^-1 ~ yr^-1)),
       title = "Relative Soil Water Content by Annual Delta SOC",
       subtitle = paste0(scenario_labels[args[3]], " | ", yrs, " years")) +
  theme_bw()