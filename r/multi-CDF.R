# filename:     multi-CDF.R    
# created:      16 July 2026
# last updated: 24 August 2026
# author:       Docker Clark

# description: This script creates and/or saves a multi-scenario CDF visualization.
# for ggplot visualization only, we take a stratified random sample of global data to reduce processing time and memory use.
#-------------------------------------------------------------------------------
# libraries 
#-------------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(stringr)
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

#output directory path
o_path <- paste(b_path, args[2], args[4], sep = "/")

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
  "ccg-ntill" = "Grass CC, No-Till & Full Residue Retention",
  "ccl-ntill" = "Legume Cover Crop, No-Tillage & Full Residue Retention")

#-------------------------------------------------------------------------------
# load in multi-cdf data table (long format) and set themes
#-------------------------------------------------------------------------------
dt_plot <- fread(paste0(o_path, "/ccg_scenarios_",     #output directory
                        gsub(" ", "_", args[6]), "_",  #region
                        args[4], ".csv"))              #timescale

#if desired, set an SOC threshold for annotation
soc.thresh <- 0.5 #in Mg / ha / yr

#create empirical cdf functions for annotation
ecdf_ccg       <- ecdf(dt_plot[scenario == "ccg", an_d_s_SOC])
ecdf_ntill     <- ecdf(dt_plot[scenario == "ntill", an_d_s_SOC])
ecdf_ccg_res   <- ecdf(dt_plot[scenario == "ccg-res", an_d_s_SOC])
ecdf_ntill_res <- ecdf(dt_plot[scenario == "ntill-res", an_d_s_SOC])
ecdf_ccg_ntill <- ecdf(dt_plot[scenario == "ccg-ntill", an_d_s_SOC])

#before random sampling, get the "n" across scenarios and display the min in the plot
unique_gridids <- dt_plot[, .(n = length(unique(gridid))), by = scenario]

if (args[6] == "Global") { #stratified random sampling to speed up plot render
  k <- 1000000 # k = desired number of samples for each stratum
  set.seed(07162026)
  dt_plot <- dt_plot[, .SD[sample(.N, min(.N, k))], by = scenario]
}

#Plot themes and labels
cdf_cols <- c("ccg-ntill" = "#2D6E56", 
              "ntill"     = "#4E9D7E", 
              "ntill-res" = "#4E9D7E", 
              "ccg"       = "#A07178", 
              "ccg-res"   = "#A07178", 
              "accent1"   = "#8A89C0", 
              "neutral1"  = "#77877B", 
              "accent2"   = "#E8A020")

cdf_lines <- c("ccg-ntill" = "solid",
               "ntill"     = "solid",
               "ccg"       = "solid",
               "ntill-res" = "dashed",
               "ccg-res"   = "dashed")

cdf_alpha <- c("ccg-ntill" = 1,
               "ntill"     = 1,
               "ccg"       = 1,
               "ntill-res" = 0.5,
               "ccg-res"   = 0.5)

ecdf_lookup <- list(
  "ccg"       = ecdf_ccg,
  "ccg-res"   = ecdf_ccg_res,
  "ntill"     = ecdf_ntill,
  "ntill-res" = ecdf_ntill_res,
  "ccg-ntill" = ecdf_ccg_ntill)

# evaluate each ecdf at soc.thresh and format as a percentage string
pct_below <- vapply(
  ecdf_lookup,
  function(f) scales::percent(f(soc.thresh), accuracy = 0.1),
  character(1))

#combine the long name (from scenario_labels) with the computed percentage names(pct_below)
legend_labels <- setNames(
  paste0(scenario_labels[names(pct_below)],
         "\nP(X≤", soc.thresh, ") = ", pct_below),
  names(pct_below))

#-------------------------------------------------------------------------------
# Plotting
#-------------------------------------------------------------------------------
CDF.plot <- ggplot(dt_plot[scenario %in% c("ccg", "ccg-res", "ntill", "ntill-res", "ccg-ntill"),]) +
  #threshold line only. per-scenario probabilities now shown in the legend text
  annotate("segment", x = soc.thresh, xend = soc.thresh, y = -Inf, yend = Inf,
           linetype = "dashed", color = cdf_cols["accent2"], linewidth = 0.8) +
  
  stat_ecdf(aes(x = an_d_s_SOC, color = scenario, linetype = scenario, alpha = scenario), 
            linewidth = 1.2) +
  
  scale_color_manual(name = "Scenario", values = cdf_cols,
                     breaks = names(legend_labels), labels = legend_labels) +
  scale_linetype_manual(name = "Scenario", values = cdf_lines,
                        breaks = names(legend_labels), labels = legend_labels) +
  scale_alpha_manual(name = "Scenario", values = cdf_alpha,
                     breaks = names(legend_labels), labels = legend_labels) +
  
  #trace out important percentiles
  geom_hline(yintercept = c(0.05, 0.5, 0.95),
             linetype = "dotted", color = "gray50", alpha = 0.6) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = paste(args[6], "CDF: Soil Carbon Change Distribution", sep = " | "),
       subtitle = paste0("Timescale: ", as.numeric(str_split(args[4], "-")[[1]][1]), " Years"),
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
    legend.position    = c(0.98, 0.02),          # x,y in [0,1], relative to panel
    legend.justification = c(1, 0),
    legend.background  = element_rect(fill = scales::alpha("white", 0.7), color = "gray80"),
    legend.title       = element_text(size = 10, face = "bold"),
    legend.text        = element_text(size = 9),
    legend.key.width   = unit(1.2, "cm")) +
  scale_x_continuous(breaks = seq(-0.5, 2.5, by = 0.5)) +
  coord_cartesian(xlim = c(-0.25, 2.5)) +
  annotate("text", x = 2.5, y = 0.95, label = 
             paste("n =", format(min(unique_gridids[ , n]), big.mark = ",")),
           hjust = 1, size = 3.5, fontface = "bold") 
  
print(CDF.plot)

#ggsave(paste0("/gpfs/scratch/docclark/woodwell/DayCent-Soil-C-Statistics/output", 
#              "/multi-CDF_", gsub(" ", "_", args[6]), "_", args[4], ".png"),
#       width = 8.5, height = 5, units = "in", dpi = 300)
