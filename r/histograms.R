# filename:     histograms.R    
# created:      03 April 2026
# last updated: 06 April 2026
# author:       Docker Clark

# description: This script produces a histogram for the ccg-ntill scenario on a 10 year timescale
#-------------------------------------------------------------------------------
# libraries
#-------------------------------------------------------------------------------

library(data.table)

#-------------------------------------------------------------------------------
# read in data
#-------------------------------------------------------------------------------
# TODO this should not be hard-coded as it as now 
#d_path <- "/gpfs/projects/McClellandGroup/projects/woodwell/DayCent-Soil-C-Statistics/data/analysis-input/10-yr/delta-cumulative-SOC-ccg-ntill.RData"

base_path <- "/gpfs/projects/McClellandGroup/projects/woodwell/DayCent-Soil-C-Statistics/data/analysis-input/10-yr"
scenario <- "delta-cumulative-SOC-ccg-ntill.RData"
load(paste(base_path, scenario, sep = "/")) # dt_scenario in GE

#-------------------------------------------------------------------------------
# stats
#-------------------------------------------------------------------------------
ccg_ntill_stats <- dt_scenario[, .(
  mean = mean(d_s_SOC),
  P0 = min(d_s_SOC),
  P25 = quantile(d_s_SOC, probs = 0.25),
  P50 = quantile(d_s_SOC, probs = 0.50),
  P75 = quantile(d_s_SOC, probs = 0.75),
  P100 = max(d_s_SOC)), 
  by = .(rep)] 

# Compute deciles
quants <- seq(0, 1, by = 0.1)
#-------------------------------------------------------------------------------
# plots
#-------------------------------------------------------------------------------
# defining x axis range (range of all values except "rep")
x_range <- range(ccg_ntill_stats[, .SD, .SDcols = !c("rep")])

# Density lines
plot(NULL, xlim = x_range, ylim = c(0, 3),
     xlab = "10-year SOC Sequestration (Mg/ha)", ylab = "Density",
     main = "Distribution of 1001 MC Summary Statistics (ccg-ntill)")
{
  lines(density(ccg_ntill_stats$mean), col = "black")
  lines(density(ccg_ntill_stats$P0),   col = "blue")
  lines(density(ccg_ntill_stats$P25),  col = "green")
  lines(density(ccg_ntill_stats$P50),  col = "orange")
  lines(density(ccg_ntill_stats$P75),  col = "red")
  lines(density(ccg_ntill_stats$P100), col = "purple")
  
}
legend("topright", legend = c("Mean", "Min", "25th", "Median", "75th", "Max"),
       col = c("black", "blue", "green", "orange", "red", "purple"), lty = 1)


# Overlapping hists #IN PROGRESS
plot(NULL, xlim = x_range, ylim = c(0, 70),
     xlab = "d_s_SOC", ylab = "Frequency",
     main = "Distribution of 1001 MC Summary Statistics (ccg-ntill)")

hist(ccg_ntill_stats$mean, col = adjustcolor("black",  alpha.f = 0.3), add = TRUE)
hist(ccg_ntill_stats$P0,   col = adjustcolor("blue",   alpha.f = 0.3), add = TRUE)
hist(ccg_ntill_stats$P25,  col = adjustcolor("green",  alpha.f = 0.3), add = TRUE)
hist(ccg_ntill_stats$P50,  col = adjustcolor("orange", alpha.f = 0.3), add = TRUE)
hist(ccg_ntill_stats$P75,  col = adjustcolor("red",    alpha.f = 0.3), add = TRUE)
hist(ccg_ntill_stats$P100, col = adjustcolor("purple", alpha.f = 0.3), add = TRUE, breaks = 50)

legend("topright", legend = c("Mean", "P0", "P25", "P50", "P75", "P100"),
       col = c("black", "blue", "green", "orange", "red", "purple"),
       pch = 15)

