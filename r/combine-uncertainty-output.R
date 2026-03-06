# file name:    combine-uncertainty-output.R
# created:      20 January 2026
# last updated: 06 March 2026
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
if (isFALSE(length(args) == 1))
  stop(
    'Needs 1 command-line argument (scenario selection).'
  )
# args[1] scenario selection
#-------------------------------------------------------------------------------
# DIRECTORIES & FILES
dir = dirname(getActiveDocumentContext()$path)
dir = str_split(dir, '/r')
dir = dir[[1]][1]
setwd(dir)
u_data    = paste(dir, 'data/uncertainty-output', sep = '/')
#-------------------------------------------------------------------------------
# LOAD FILES by crop, irrigation
#-------------------------------------------------------------------------------
dt_somsc = list.files(u_data, args[1], full.names = TRUE)
dt_somsc = lapply(dt_somsc, fread)
dt_somsc = rbindlist(dt_somsc)
gc()
#-------------------------------------------------------------------------------
# SAVE FILE
#-------------------------------------------------------------------------------
# save as Rdata
save(dt_somsc, file = paste0(u_data, '/',args[1],'-somsc-uncertainty.RData'))
