# filename:     histograms.R    
# created:      03 April 2026
# last updated: 09 April 2026
# author:       Docker Clark

# description: This script computes statistics and makes a histogram for scenarios on a 10 or 20-yr timescale.
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
args[1] <- 'ntill-res'
args[2] <- '10-yr'
args[3] <- '/gpfs/projects/McClellandGroup/projects/woodwell/DayCent-Soil-C-Statistics/data/analysis-input'
args[4] <- "delta-cumulative-SOC-"
#check if there's enough info to get a filepath
#if (isFALSE(length(args) == 4)) stop( 'Needs 3 command-line argument (scenario selection, timeframe, data path).' )

#get current directory (dir)
dir = dirname(getActiveDocumentContext()$path) 
dir = str_split(dir, '/r') 
dir = dir[[1]][1] 
setwd(dir) # set as working directory

#set input data directory
d_dir <- paste(args[3], args[2], sep = '/')
#set output data directory
o_dir <- paste(args[3], args[2], sep = '/')

#-------------------------------------------------------------------------------
# read in data
#-------------------------------------------------------------------------------

load(paste0(args[3], "/", args[2], "/", args[4], args[1],".RData")) # dt_scenario in GE
#-------------------------------------------------------------------------------
# stats
#-------------------------------------------------------------------------------
#annualize SOC 
dt_scenario[, d_s_SOC := d_s_SOC / 10]

dt_scenario_stats <- dt_scenario[, .(
  Min = min(d_s_SOC),
  P25 = quantile(d_s_SOC, probs = 0.25),
  Median = quantile(d_s_SOC, probs = 0.50),
  Mean = mean(d_s_SOC),
  P75 = quantile(d_s_SOC, probs = 0.75),
  Max = max(d_s_SOC)), 
  by = .(rep)] 

#-------------------------------------------------------------------------------
# plots
#-------------------------------------------------------------------------------
linecols <- viridis::viridis(6)
#PDFs of summary stats as a ridgeline plot

#make data long for read-in to ggplot
dt_long <- melt(dt_scenario_stats, 
                measure.vars = setdiff(colnames(dt_scenario_stats), "rep"),
                variable.name = "statistic",
                value.name = "SOC")


#ridgeline plot
ggplot(dt_long, aes(x = SOC, y = statistic, fill = statistic)) +
  geom_density_ridges(alpha = 0.6, rel_min_height = 0.01, #cut off lines <0.1%
                      color = "gray20", linewidth = 0.4) +
  scale_fill_manual(values = linecols) +
  labs(x = paste0("Annual Mg/ha SOC Sequestration Over 10 Years (", args[1], ")"),
       y = NULL,
       title = "Distribution of Summary Statistics",
       subtitle = paste0("Scenario - ", args[1]),
       caption = "Variation from 1,001 Monte Carlo draws") +
  theme_ridges(font_family = "sans") +
  theme(
    legend.position    = "none",
    plot.title         = element_text(size = 13, face = "bold"),
    plot.caption       = element_text(size = 8, color = "grey50"),
    axis.text          = element_text(size = 10),
    axis.title.x       = element_text(size = 11),
    panel.grid.major.x = element_line(color = "grey90"),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.margin        = margin(15, 15, 10, 10)
  )

#PDF
ggplot(dt_scenario, aes(x = d_s_SOC)) +
  geom_density(fill = "#4e9d7e", color = "#2d6e56",
               alpha = 0.6, linewidth = 0.8) +
  #geom_rug(alpha = 0.3, length = unit(0.03, "npc")) +
  labs(title = "PDF: Soil Carbon Change Distribution",
       subtitle = paste("Scenario:", args[1]),
       x = "Soil Carbon Change (Mg C/ha)",
       y = "Probability Density") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.line = element_line(color = "grey70"))

# CDF - shows cumulative probabilities
ggplot(dt_scenario, aes(x = d_s_SOC)) +
  stat_ecdf(geom = "step", linewidth = 1.2, color = "#2d6e56") +
  #geom_vline(xintercept = 0.5, linetype = "dashed", color = "lightgreen", linewidth = 0.8) +
  geom_hline(yintercept = c(0.05, 0.5, 0.95),
             linetype = "dotted", color = "gray50", alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "CDF: Soil Carbon Change Distribution",
       subtitle = paste("Scenario:", args[1]),
       x = "Soil Carbon Change (Mg C/ha)",
       y = "Cumulative Probability") +
  theme_bw()


