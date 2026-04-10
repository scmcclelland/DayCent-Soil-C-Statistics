# filename:     histograms.R    
# created:      03 April 2026
# last updated: 10 April 2026
# author:       Docker Clark

# description: This script computes statistics and makes a visualizations for scenarios on a 10 or 20-yr timescale.
#-------------------------------------------------------------------------------
# libraries 
#-------------------------------------------------------------------------------

library(data.table)
library(rstudioapi)
library(ggplot2)
library(stringr)
library(ggridges)

#-------------------------------------------------------------------------------
# directories and startup 
#-------------------------------------------------------------------------------
args     = commandArgs(trailingOnly = TRUE) 
#these can be updated for different scenarios
args[1] <- 'ccg'
args[2] <- '10-yr'
args[3] <- '/gpfs/projects/McClellandGroup/projects/woodwell/DayCent-Soil-C-Statistics/data'
args[4] <- 'analysis-input'
args[5] <- 'analysis-output'
args[6] <- "delta-cumulative-SOC"
#check if there's enough info to get a filepath
if (isFALSE(length(args) == 6)) stop( 'Needs 6 command-line argument (scenario selection, timeframe, data path,
                                      input/output, data file header).' )

#get current directory (dir)
dir = dirname(getActiveDocumentContext()$path) 
dir = str_split(dir, '/r') 
dir = dir[[1]][1] 
setwd(dir) # set as working directory

#set input data directory
d_dir <- paste(args[3], args[4], args[2], sep = '/')
#set output data directory
o_dir <- paste(args[3], args[5], args[2], sep = '/')


#-------------------------------------------------------------------------------
# read in data
#-------------------------------------------------------------------------------

load(paste0(args[3], "/", args[4], "/",     #base file path
            args[2], "/", args[6], "-",     #time scale & SOC delta
            args[1],".RData"))              #scenario code

#-------------------------------------------------------------------------------
# data manipulation
#-------------------------------------------------------------------------------
#annualize SOC 
dt_scenario[, d_s_SOC := d_s_SOC / 10]

dt_stats <- dt_scenario[, .( # <<<FIRST CHANGE HERE
  Min = min(d_s_SOC),
  P25 = quantile(d_s_SOC, probs = 0.25),
  Median = quantile(d_s_SOC, probs = 0.50),
  Mean = mean(d_s_SOC),
  P75 = quantile(d_s_SOC, probs = 0.75),
  Max = max(d_s_SOC)), 
  by = .(rep)] 

#melt data to long for read-in to ggplot
dt_long <- melt(dt_stats,
                measure.vars = setdiff(colnames(dt_stats), "rep"),
                variable.name = "statistic",
                value.name = "SOC")

#a function which allows calculation of probabilities from PDFs and CDFs
ecdf_fn <- ecdf(dt_scenario$d_s_SOC)

#create dt for mean/med comparison between scenarios
if (!exists("dt_scenario_means")) {
  dt_scenario_means <- data.table()
}
if (!exists("dt_scenario_meds")) {
  dt_scenario_meds <- data.table()
}

#pull out desired summary stats
dt_scenario_means[, (args[1]) := dt_stats$Mean]
dt_scenario_meds[, (args[1]) := dt_stats$Median]

#melt to long form for ggplot once enough scenarios in dt
dt_means_long <- melt(dt_scenario_means, 
                      measure.vars = colnames(dt_scenario_means),
                      variable.name = "scenario",
                      value.name = "mean_SOC")
dt_meds_long <- melt(dt_scenario_meds, 
                      measure.vars = colnames(dt_scenario_meds),
                      variable.name = "scenario",
                      value.name = "med_SOC")
#-------------------------------------------------------------------------------
# shared themes (visualization)
#-------------------------------------------------------------------------------
#color scheme
linecols <- viridis::viridis(6)

#create a key for the scenario codes
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

#would be a simple matter to save certain themes like those in plot.title
#element_text(size = 13, face = "bold") for reuse in multiple plots
#-------------------------------------------------------------------------------
# ridgeline plot (summary stats)
#-------------------------------------------------------------------------------
ggplot(dt_long, aes(x = SOC, y = statistic, fill = statistic)) +
  geom_density_ridges(alpha = 0.6, rel_min_height = 0.01,
                      color = "gray20", linewidth = 0.4,
                      bandwidth = 0.035) + #binwidth for smoothing
  scale_fill_manual(values = linecols) +
  labs(x = expression("Mg ha"^-1~"y"^-1~"SOC Sequestration Over 10 Years"),
       y = NULL,
       title = "Distribution of Summary Statistics",
       subtitle = paste0("Scenario - ", scenario_labels[args[1]]),
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

#create a filename to save this plot as
#fname_ridgeline <- paste("Ridgeline", args[2], args[1], sep = "_")
#save to output directory
#ggsave(filename = paste0(o_dir, "/", fname_ridgeline, ".tiff"),
#       units    = "in",
#       width    = 7,
#       height   = 5,
#       dpi      = 300,
#       bg       = "white")

#-------------------------------------------------------------------------------
# PDF: Probability Density Function
#-------------------------------------------------------------------------------
{#specify a probability range to highlight if desired. otherwise skip
  prob_range <- ecdf_fn(0.5) - ecdf_fn(0.25)
  #precompute density so we can shade a region
  dens <- density(dt_scenario$d_s_SOC, adjust = 2)
  dens_dt <- data.table(x = dens$x, y = dens$y)
}

PDF.plot <- ggplot(dt_scenario, aes(x = d_s_SOC)) +
  geom_density(fill = "#4e9d7e", color = "#2d6e56",
               alpha = 0.6, linewidth = 0.8,
               adjust = 2) +
  labs(title = "PDF: Soil Carbon Change Distribution",
       subtitle = paste("Scenario:", scenario_labels[args[1]]),
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
  
#create a filename for saving
#fname_PDF <- paste("Prob_Dens", args[2], args[1], sep = "_")
#save to output directory
#ggsave(filename = paste0(o_dir, "/", fname_PDF, ".tiff"),
#       units    = "in",
#       width    = 7,
#       height   = 5,
#       dpi      = 300,
#       bg       = "white")

#-------------------------------------------------------------------------------
# CDF: Cumulative Density Function
#-------------------------------------------------------------------------------
# todo- add an option to choose a threshold to then mark on the CDF plot
ggplot(dt_scenario, aes(x = d_s_SOC)) +
  stat_ecdf(geom = "step", linewidth = 1.2, color = "#2d6e56") +
  geom_hline(yintercept = c(0.05, 0.5, 0.95),
             linetype = "dotted", color = "gray50", alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "CDF: Soil Carbon Change Distribution",
       subtitle = paste("Scenario:", scenario_labels[args[1]]),
       x = expression("Soil Carbon Change (Mg C ha"^-1~"y"^-1*")"),
       y = "Cumulative Probability") +
  theme_bw() +
  theme(
    plot.title      = element_text(size = 13, face = "bold"),
    plot.subtitle   = element_text(size = 11),
    axis.text       = element_text(size = 10),
    axis.title      = element_text(size = 11),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(15, 15, 10, 10)
  )
#file name for saving
#fname_CDF <- paste("Cumul_Dens", args[2], args[1], sep = "_")
#save to output directory
#ggsave(filename = paste0(o_dir, "/", fname_CDF, ".tiff"),
#       units    = "in",
#       width    = 7,
#       height   = 5,
#       dpi      = 300,
#       bg       = "white")

#-------------------------------------------------------------------------------
#Histograms
#-------------------------------------------------------------------------------
#shared-axis hist of different scenario MEANS
ggplot(dt_means_long, aes(x = mean_SOC, fill = scenario)) +
  geom_histogram(alpha = 0.5, bins = 150, position = "identity") +
  scale_fill_manual(values = linecols, labels = scenario_labels) +
  labs(x = expression("Mean SOC Sequestration (Mg ha"^-1~"yr"^-1*")"),
       y = "Frequency",
       fill = "Scenario",
       title = "Distribution of Scenario Means") +
  theme_classic() +
  theme(legend.position = c(0.65, 0.75),
        legend.background = element_rect(fill = "white", color = "grey90"))

#file name for saving
#fname_mean_hist <- paste("Scenario_hist", args[2], 
#                         paste(colnames(dt_scenario_means), collapse = "_"), 
#                         sep = "_")
#save to output directory
#ggsave(filename = paste0(o_dir, "/", fname_mean_hist, ".tiff"),
#       units    = "in",
#       width    = 7,
#       height   = 5,
#       dpi      = 300,
#       bg       = "white")

#shared-axis hist of different scenario MEDIANS
ggplot(dt_meds_long, aes(x = med_SOC, fill = scenario)) +
  geom_histogram(alpha = 0.5, bins = 150, position = "identity") +
  scale_fill_manual(values = linecols, labels = scenario_labels) +
  labs(x = expression("Median SOC Sequestration (Mg ha"^-1~"yr"^-1*")"),
       y = "Frequency",
       fill = "Scenario",
       title = "Distribution of Scenario Medians") +
  theme_classic() +
  theme(legend.position = c(0.65, 0.75),
        legend.background = element_rect(fill = "white", color = "grey90"))
#file name for saving
#fname_med_hist <- paste("Scenario_hist", args[2], 
#                         paste(colnames(dt_scenario_meds), collapse = "_"), 
#                         sep = "_")
#save to output directory
#ggsave(filename = paste0(o_dir, "/", fname_med_hist, ".tiff"),
#       units    = "in",
#       width    = 7,
#       height   = 5,
#       dpi      = 300,
#       bg       = "white")

#-------------------------------------------------------------------------------
# Output
#-------------------------------------------------------------------------------
#save the stats from each scenario to the output directory
fwrite(x    = dt_stats,
  file      = paste0(o_dir, "/", args[2], "_", args[1], "_stats.csv"),
  sep       = ",")