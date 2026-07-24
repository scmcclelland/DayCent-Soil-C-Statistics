# file name:    summary-table.R
# created:      24 July 2026
# last updated: 24 July 2026
# author:       Docker Clark

# description: This script creates and outputs a table of global and regional means for various scenarios.
# dependencies: this script requires a table WB_dt created by the geo-analysis.R script
#-------------------------------------------------------------------------------
# libraries 
#-------------------------------------------------------------------------------

library(data.table)
library(rstudioapi)
library(stringr)

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

# Add desired regions
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

# create regional lookup table
region_dt <- rbindlist(
  lapply(names(regions), function(r) data.table(region = r, WB_NAME = regions[[r]])))

#-------------------------------------------------------------------------------
# Generate regional means
#-------------------------------------------------------------------------------
# this loop loads-in data for desired scenarios and appends world bank names and regions
# for later filtration. Note: resultant tables contain duplicate rows and must be filtered
# by region.
table_scenarios <- c("ccg", "ccg-res", "ccg-ntill")
for (s in table_scenarios) {
  #load in as dt_scenario
  load(paste0(b_path, "/", args[1], "/",      #base file path
              args[4], "/", args[5], "-",     #time scale & SOC delta
              s,".RData"))                    #scenario code and extension
  message(paste0("Loaded ", scenario_labels[s]))
  
  #annualize SOC as a new column so either can be used
  yrs <- as.numeric(str_split(args[4], "-")[[1]][1])
  dt_scenario[, an_d_s_SOC := d_s_SOC / yrs]
  
  # join country data table to simulation data
  dt_scenario <- WB_dt[, c('cell', 'WB_NAME', 'x', 'y')][dt_scenario, on = .(cell = gridid)]
  
  #rename cell to avoid confusion
  setnames(dt_scenario, "cell", "gridid")
  setorder(dt_scenario, gridid)
  gc() #garbage collection
  
  #allow.cartesian allows for rows to be added when a WB_NAME belongs two region groups
  # ex. France now has duplicate rows labeled "Global" and "European Union"
  dt_scenario <- merge(dt_scenario, region_dt, by = "WB_NAME", allow.cartesian = TRUE)
  
  message("Calculating means by region")
  dt_scenario <- dt_scenario[, .(
    Mean   = mean(an_d_s_SOC)), 
    by = .(region, rep)] #must include by = region
  
  #rename it according to s
  assign(paste0("dt_means_", gsub("-", "_", s)), dt_scenario)
  
  #remove dt_scenario on last iteration
  if (s == table_scenarios[length(table_scenarios)]) {rm(dt_scenario)}
}

#-------------------------------------------------------------------------------
# Build summary table
#-------------------------------------------------------------------------------
sum_table <- data.table()

sum_table <- rbindlist(lapply(table_scenarios, function(s) {
  dt <- get(paste0("dt_means_", gsub("-", "_", s)))
  dt[, .(
    Mean  = round(mean(Mean), 3),
    Lower = round(quantile(Mean, 0.025), 3),
    Upper = round(quantile(Mean, 0.975), 3)
  ), by = region][, scenario := scenario_labels[s]]
}))

# combine mean + interval into one formatted string per cell
sum_table[, cell_value := paste0(Mean, "\n(", Lower, " - ", Upper, ")")]

# reshape long -> wide: scenarios as rows, regions as columns
sum_table <- dcast(sum_table, scenario ~ region, value.var = "cell_value")

#output
fwrite(sum_table, paste0(b_path, "/", args[2], "/", args[4], "/",
                         "Regional_means_table.csv"))