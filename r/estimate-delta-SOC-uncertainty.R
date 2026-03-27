# file name:    estimate-delta-SOC-uncertainty.R
# created:      27 March 2026
# last updated: 27 March 2026
# author:       S.C. McClelland

# description: This file estimate delta SOC (with uncertainty) between scenario and conv.
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
if (isFALSE(length(args) == 2))
  stop(
    'Needs 2 command-line argument (scenario selection).'
  )
# args[1] scenario selection
# args[2] timeframe (10-yr or 20-yr)
#-------------------------------------------------------------------------------
# FUNCTIONS 
#-------------------------------------------------------------------------------
delta_soc       = function(delta_somsc) { # positive output indicates soil C gain
  Mg_ha  = 100
  delta_soc     = delta_somsc/Mg_ha 
}
#-------------------------------------------------------------------------------
# DIRECTORIES 
#-------------------------------------------------------------------------------
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)
u_data    = paste0(dir, '/data/uncertainty-output/', args[2], '/merged')
o_data    = paste(dir, 'data/analysis-input', sep = '/')
#-------------------------------------------------------------------------------
# LOAD CONV 
#-------------------------------------------------------------------------------
load(paste(u_data, 'uncertainty-conv-somsc.RData', sep = '/'))
# rename
dt_conv = copy(dt_somsc)
rm(dt_somsc)
gc()
# unit conversion
dt_conv[, s_SOC   := delta_soc(s_somsc)]
dt_conv[, s_somsc := NULL]
gc()
#-------------------------------------------------------------------------------
# LOAD SCENARIO 
#-------------------------------------------------------------------------------
load(paste0(u_data, '/', args[1], '.RData'))
# unit conversion
dt_somsc[, s_SOC   := delta_soc(s_somsc)]
dt_somsc[, s_somsc := NULL]
gc()
#-------------------------------------------------------------------------------
# ESTIMATE DELTA 
#-------------------------------------------------------------------------------
dt_scenario = dt_somsc[dt_conv[, -c('scenario')], on = .(gridid   = gridid,
                                                    crop          = crop,
                                                    irr           = irr,
                                                    ssp           = ssp,
                                                    gcm           = gcm,
                                                    time          = time,
                                                    rep           = rep)]
rm(dt_somsc, dt_conv)
gc()
# d_s_SOC
dt_scenario[, d_s_SOC := s_SOC - i.s_SOC]
dt_scenario[, i.s_SOC := NULL]
# remove absolute estimate
dt_scenario[, s_SOC := NULL]
# remove NA
dt_scenario = dt_scenario[!is.na(scenario)]
gc()
#-------------------------------------------------------------------------------
# SAVE 
#-------------------------------------------------------------------------------
save(dt_scenario, file = paste0(o_data, '/', args[2],'/delta-cumulative-SOC-', 
                                unique(dt_scenario[,scenario]), 
                                '.RData'))
