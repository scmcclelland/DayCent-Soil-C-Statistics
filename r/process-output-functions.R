# DayCent Simulation Model Output Functions for Results Processing for Analysis #

# file name:    process-output-functions.R
# author:       S.C. McClelland
# created:      24 July 2023
# last updated: 20 January 2026
#-----------------------------------------------------------------------------------------
# LIBRARIES
#-----------------------------------------------------------------------------------------
library(data.table)
library(sf)
library(terra)
options(scipen = 999, digits = 4)
#-----------------------------------------------------------------------------------------
# FUNCTIONS
#-----------------------------------------------------------------------------------------
# process SOC
# .dt is data table
# .scen is selected scenarios
# .time is final simulation year
process_SOC   = function(.dt, .scen, .time) { 
  if (.scen == 'rewild') { # get fsysc for rewild
    # select relevant columns
    cols    = c('gridid', 'crop', 'scenario', 'irr', 'ssp', 'gcm', 'time', 'somsc', 'fsysc')
    file1   = .dt[scenario == .scen, ..cols]
    gc()
  } else {
    # select relevant columns
    cols    = c('gridid', 'crop', 'scenario', 'irr', 'ssp', 'gcm', 'time', 'somsc')
    file1   = .dt[scenario == .scen, ..cols]
    gc()
  }

  # make unique
  file1 = unique(file1)
  
  # select 2016-.time
  file1 = file1[time <= .time,]
  gc()
  
  # add run year information
  file1[, run_yrs := 1L]
  file1[, run_yrs := sum(run_yrs), by = .(gridid, crop, scenario, irr, ssp, gcm)]
  gc()
  
  
  setcolorder(file1, c('gridid', 'crop', 'scenario', 'irr', 'ssp', 'gcm', 'time', 
              'run_yrs', 'somsc'))
  print(colnames(file1))
  print(head(file1))
  return(file1)
}
