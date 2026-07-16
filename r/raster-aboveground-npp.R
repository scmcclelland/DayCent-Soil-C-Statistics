# file name:    raster-aboveground-npp.R
# created:      07 July 2026
# last updated: 07 July 2026
# author:       S.C. McClelland

# description: This file rasterizes aboveground NPP removal. Run manually.
#-------------------------------------------------------------------------------
# LIBRARIES
#-------------------------------------------------------------------------------
library(data.table)
library(rstudioapi)
library(stringr)
library(terra)
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
npp_data   = paste0(dir, '/data/model-output/harvest-index/calculated')
in_data    = paste0(dir, '/data/analysis-input')
#-------------------------------------------------------------------------------
# LOAD FILES 
#-------------------------------------------------------------------------------
# load estimates
# load crop mask
#-------------------------------------------------------------------------------
# CLEAN 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# RASTERIZE 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# SAVE RASTER
#-------------------------------------------------------------------------------

