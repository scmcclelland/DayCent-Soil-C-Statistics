# filename:     histograms.R    
# created:      03 April 2026
# last updated: 03 April 2026
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

ccg_ntill_mean[, d_s_SOC_scaled := d_s_SOC / 10]

quantile(ccg_ntill_mean$d_s_SOC)
quantile(ccg_ntill_mean$d_s_SOC_scaled)
#-------------------------------------------------------------------------------
# plot
#-------------------------------------------------------------------------------
hist(ccg_ntill_mean$d_s_SOC, breaks = 150,
     main = "Distribution of 10-year MC Means (ccg-ntill)")

abline(v = c(quantile(ccg_ntill_mean$d_s_SOC)), #add lines for quantiles
       col = "red",
       lwd = 2,
       lty = 2)
# Now for the scaled column
hist(ccg_ntill_mean$d_s_SOC_scaled, breaks = 150,
     main = "Distribution of 10-year MC Means (ccg-ntill)")

abline(v = c(quantile(ccg_ntill_mean$d_s_SOC_scaled)), #add lines for quantiles
       col = "red",
       lwd = 2,
       lty = 2)

