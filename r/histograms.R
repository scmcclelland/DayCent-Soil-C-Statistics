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
#d_path <- "/gpfs/projects/McClellandGroup/projects/woodwell/DayCent-Soil-C-Statistics/data/analysis-input/10-yr/"
load("/gpfs/projects/McClellandGroup/projects/woodwell/DayCent-Soil-C-Statistics/data/analysis-input/10-yr/delta-cumulative-SOC-ccg-ntill.RData")

#-------------------------------------------------------------------------------
# stats
#-------------------------------------------------------------------------------
ccg_ntill_mean <- dt_scenario[, lapply(.SD, mean), .SDcols = "d_s_SOC", by = .(rep)]
# .SD stands for Subset of Data — it's a data.table keyword that refers to the columns specified in .SDcols

# Min (P0)
ccg_ntill_P0 <- dt_scenario[, lapply(.SD, function(x) {min(x)}),
                             .SDcols = 'd_s_SOC',
                             by = .(rep)]

# 25th percentile (P25)
ccg_ntill_P25 <- dt_scenario[, lapply(.SD, function(x) {quantile(x, probs = 0.25)}),
           .SDcols = 'd_s_SOC',
           by = .(rep)]

# 50th percentile (P50)
ccg_ntill_P50 <- dt_scenario[, lapply(.SD, function(x) {quantile(x, probs = 0.50)}),
                            .SDcols = 'd_s_SOC',
                            by = .(rep)] 

# 75th percentile (P75)
ccg_ntill_P75 <- dt_scenario[, lapply(.SD, function(x) {quantile(x, probs = 0.75)}),
                             .SDcols = 'd_s_SOC',
                             by = .(rep)] 

# Max (P100)
ccg_ntill_P100 <- dt_scenario[, lapply(.SD, function(x) {max(x)}),
                            .SDcols = 'd_s_SOC',
                            by = .(rep)]

#ccg_ntill_mean[, d_s_SOC_scaled := d_s_SOC / 10] #add a scaled column for per year soc
quantile(ccg_ntill_mean$d_s_SOC)

# Compute deciles
quants <- seq(0, 1, by = 0.1)
#-------------------------------------------------------------------------------
# plots
#-------------------------------------------------------------------------------
# Mean
hist(ccg_ntill_mean$d_s_SOC, breaks = 150,
     main = "Distribution of 10-year MC Means (ccg-ntill)")

abline(v = c(quantile(ccg_ntill_mean$d_s_SOC, probs = quants)), #add lines for quantiles
       col = "red",
       lwd = 2,
       lty = 2)

#P0
hist(ccg_ntill_P0$d_s_SOC, breaks = 150,
     main = "Distribution of 10-year MC SOC Delta (Mins)")
abline(v = c(quantile(ccg_ntill_P0$d_s_SOC, probs = quants)),
       col = "red",
       lwd = 2,
       lty = 2)
# P25
hist(ccg_ntill_P25$d_s_SOC, breaks = 150,
     main = "Distribution of 10-year MC SOC Delta (25th Percentile)")
abline(v = c(quantile(ccg_ntill_P25$d_s_SOC, probs = quants)),
       col = "red",
       lwd = 2,
       lty = 2)

#P50
hist(ccg_ntill_P50$d_s_SOC, breaks = 150,
     main = "Distribution of 10-year MC SOC Delta (50th Percentile)")
abline(v = c(quantile(ccg_ntill_P50$d_s_SOC, probs = quants)),
       col = "red",
       lwd = 2,
       lty = 2)

#P75
hist(ccg_ntill_P75$d_s_SOC, breaks = 150,
     main = "Distribution of 10-year MC SOC Delta (75th Percentile)")
abline(v = c(quantile(ccg_ntill_P75$d_s_SOC, probs = quants)),
       col = "red",
       lwd = 2,
       lty = 2)

#P100
hist(ccg_ntill_P100$d_s_SOC, breaks = 150,
     main = "Distribution of 10-year MC SOC Delta (Maxes)")
abline(v = c(quantile(ccg_ntill_P100$d_s_SOC, probs = quants)),
       col = "red",
       lwd = 2,
       lty = 2)

