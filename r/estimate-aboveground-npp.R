# file name:    estimate-aboveground-npp.R
# created:      07 July 2026
# last updated: 07 July 2026
# author:       S.C. McClelland

# description: This file estimates aboveground NPP removal. Run manually.
#-------------------------------------------------------------------------------
# LIBRARIES
#-------------------------------------------------------------------------------
library(data.table)
library(rstudioapi)
library(stringr)
#-------------------------------------------------------------------------------
# ARGS
#-------------------------------------------------------------------------------
args   = commandArgs(trailingOnly = TRUE)
if (isFALSE(length(args) == 1))
  stop(
    'Needs 2 command-line argument (scenario selection).'
  )
# args[1] [scenario selection]
#-------------------------------------------------------------------------------
# DIRECTORIES & FILES
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)
hi_data    = paste0(dir, '/data/model-output/harvest-index/output')
npp_data   = paste0(dir, '/data/model-output/harvest-index/calculated')
in_data    = paste0(dir, '/data/analysis-input')
#-------------------------------------------------------------------------------
# LOAD FILES 
#-------------------------------------------------------------------------------
dt_HI = fread(paste0(hi_data, '/historical-', args[1], '-HI-results.csv'))
# residue retention fraction data
load(paste(in_data, 'input_table_by_gridid_crop_irr.RData', sep = '/'))
main_table = main_table[, .(gridid, crop, irr, res.rtrn.amt)]
#-------------------------------------------------------------------------------
# CLEAN 
#-------------------------------------------------------------------------------
if(args[1] %like% 'cc') {
  print('Cleaning table to separate cover crop aboveground biomass.')
  # separate cc biomass
  dt_HI_cc = dt_HI[crpval == "'RYE78'",]
  dt_HI_cc = dt_HI_cc[, .(gridid, crop, scenario, irr, ssp, gcm, time, run_yrs, agcacc)]
  dt_HI_cc[, run_yrs := NULL]
  # subset to 2017-2036 (for exactly 20 years to match SOC)
  dt_HI_cc = dt_HI_cc[!time == 2016,]
  # recalculate run years
  dt_HI_cc[, run_yrs := 1L]
  dt_HI_cc[, run_yrs := sum(run_yrs), by = .(gridid, crop, scenario, irr, ssp, gcm)]
  # keep only 20 run years
  dt_HI_cc = dt_HI_cc[run_yrs == 20,]
  dt_HI_cc[, run_yrs := NULL]
  
  dt_HI = dt_HI[!crpval == "'RYE78'",]
  # restrict to 21 run years (2016-2036)
  dt_HI = dt_HI[run_yrs == 21,]
  dt_HI[, run_yrs := NULL]
  # subset to 2017-2036 (for exactly 20 years to match SOC)
  dt_HI = dt_HI[!time == 2016,]
  # remove cgrain == 0 cases
  dt_HI = dt_HI[cgrain > 0,]
  # recalculate run years
  dt_HI[, run_yrs := 1L]
  dt_HI[, run_yrs := sum(run_yrs), by = .(gridid, crop, scenario, irr, ssp, gcm)]
  # keep only 20 run years
  dt_HI = dt_HI[run_yrs == 20,]
  dt_HI[, run_yrs := NULL]
  # note: some years report 0 hi, returning undefined value in calculation step
  dt_HI[, crpval := NULL]
  dt_HI[, agcacc := NULL]
  # rejoin dt
  dt_HI = dt_HI[dt_HI_cc, on = .(gridid = gridid,
                                 crop   = crop,
                                 scenario = scenario,
                                 irr      = irr,
                                 ssp      = ssp,
                                 gcm      = gcm,
                                 time     = time)]
  dt_HI = dt_HI[!is.na(hi)]
  rm(dt_HI_cc)
  
} else {
  print('This is not a cover crop scenario.')
  # restrict to 21 run years (2016-2036)
  dt_HI = dt_HI[run_yrs == 21,]
  dt_HI[, run_yrs := NULL]
  # subset to 2017-2036 (for exactly 20 years to match SOC)
  dt_HI = dt_HI[!time == 2016,]
  # remove cgrain == 0 cases
  dt_HI = dt_HI[cgrain > 0,]
  # recalculate run years
  dt_HI[, run_yrs := 1L]
  dt_HI[, run_yrs := sum(run_yrs), by = .(gridid, crop, scenario, irr, ssp, gcm)]
  # keep only 20 run years
  dt_HI = dt_HI[run_yrs == 20,]
  dt_HI[, run_yrs := NULL]
  # note: some years report 0 hi, returning undefined value in calculation step
}
#-------------------------------------------------------------------------------
# ESTIMATE 
#-------------------------------------------------------------------------------
# join with res removal fraction
if(args[1] == 'conv') {
  dt_HI = dt_HI[main_table, on = .(gridid = gridid,
                                   crop   = crop,
                                   irr    = irr)]
  dt_HI = dt_HI[!is.na(scenario)]
  dt_HI[, fr := 1-res.rtrn.amt]
} else {
  print('This scenario includes 100% residue retention.')
  dt_HI[, fr := 0]
}
# calculate by crop, irr, time
dt_HI[, e_aglivc   := cgrain/hi]
dt_HI[, e_agresc   := e_aglivc - cgrain]
dt_HI[, e_agresc_r := e_agresc*fr]
dt_HI[, e_abgr     := cgrain + e_agresc_r]
if (args[1] %like% 'cc') {
  dt_HI[, e_abgNPP_r := (e_abgr / (e_aglivc + agcacc))*100] # as percentage
  # mean by crop, irr
  dt_HI_ci_mn = dt_HI[, lapply(.SD, mean, na.rm = TRUE), .SDcols = 'e_abgNPP_r', by = .(gridid, crop, scenario, irr)]
  # mean for gridid
  dt_HI_mn = dt_HI_ci_mn[, lapply(.SD, mean, na.rm = TRUE), .SDcols = 'e_abgNPP_r', by = .(gridid, scenario)]
} else {
  dt_HI[, e_abgNPP_r := (e_abgr / e_aglivc)*100] # as percentage
  # mean by crop, irr
  dt_HI_ci_mn = dt_HI[, lapply(.SD, mean, na.rm = TRUE), .SDcols = 'e_abgNPP_r', by = .(gridid, crop, scenario, irr)]
  # mean for gridid
  dt_HI_mn = dt_HI_ci_mn[, lapply(.SD, mean, na.rm = TRUE), .SDcols = 'e_abgNPP_r', by = .(gridid, scenario)]
  
}
#-------------------------------------------------------------------------------
# SAVE DT
#-------------------------------------------------------------------------------
fwrite(dt_HI_mn, file = paste0(npp_data, '/estimated-abg-NPP-removed-',args[1],'.csv'))
