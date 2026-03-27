# file name:    combine-uncertainty-output.R
# created:      20 January 2026
# last updated: 27 March 2026
# author:       S.C. McClelland

# description: This file combines uncertainty output by scenario.
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
# DIRECTORIES & FILES
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)
u_data    = paste0(dir, '/data/uncertainty-output/', args[2])
#-------------------------------------------------------------------------------
# LOAD FILES by crop, irrigation
#-------------------------------------------------------------------------------
dt_somsc = list.files(u_data, args[1], full.names = TRUE)
if(args[1] == 'uncertainty-ntill') {dt_somsc = dt_somsc[!dt_somsc %like% 'res']}
if(args[1] == 'uncertainty-ccg') {dt_somsc   = dt_somsc[!dt_somsc %like% 'res']
                                  dt_somsc   = dt_somsc[!dt_somsc %like% 'ntill']}
if(args[1] == 'uncertainty-ccl') {dt_somsc   = dt_somsc[!dt_somsc %like% 'res']
                                  dt_somsc   = dt_somsc[!dt_somsc %like% 'ntill']}
dt_somsc = lapply(dt_somsc, fread)
dt_somsc = rbindlist(dt_somsc)
gc()
#-------------------------------------------------------------------------------
# SAVE FILE
#-------------------------------------------------------------------------------
# save as Rdata
save(dt_somsc, file = paste0(u_data, '/merged/',args[1],'-somsc.RData'))
