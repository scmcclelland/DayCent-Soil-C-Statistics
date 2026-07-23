# file name:    raster-aboveground-npp.R
# created:      07 July 2026
# last updated: 20 July 2026
# author:       S.C. McClelland

# description: This file rasterizes aboveground NPP removal. Run manually.
#-------------------------------------------------------------------------------
# LIBRARIES
#-------------------------------------------------------------------------------
library(data.table)
library(rstudioapi)
library(stringr)
library(terra)
library(sf)
options(scipen = 999, digits = 4)
#-------------------------------------------------------------------------------
# ARGS
#-------------------------------------------------------------------------------
args   = commandArgs(trailingOnly = TRUE)
if (isFALSE(length(args) == 1))
  stop(
    'Needs 1 command-line argument (scenario selection).'
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
o_data     = paste0(npp_data, '/rasters')
#-------------------------------------------------------------------------------
# LOAD FILES 
#-------------------------------------------------------------------------------
# estimates
abg_npp_dt = fread(paste0(npp_data, '/estimated-abg-NPP-removed-', args[1], '.csv'))
# crop mask (for target dimensions only)
crop_r = rast(paste(in_data, 'msw-cropland-rf-ir-area.tif', sep = '/'))
#-------------------------------------------------------------------------------
# RASTERIZE 
#-------------------------------------------------------------------------------
# make target r
t_r           = crop_r[[1]]
values(t_r)   = NA_real_
varnames(t_r) = ''
names(t_r)    = ''

# original data
# l1
r1                = t_r
grs1              = abg_npp_dt[, gridid]
values(r1)[grs1]  = abg_npp_dt[, e_abgNPP_r]
names(r1)         = 'model_abgNPP_r'
# conv min 9.7, max 91.3
# res min 3.2, max 68.0
# ccg-ntill min 0, 56.5

# DEPRECATED
# add land mask to layer 1
# land_m = ifel(crop_r[[1]] >= 0, 0, NA) # select any layer
# r1     = cover(r1, land_m)

# crop mask
# l2
# count how many crop layers contain valid data in each cell
n_valid = app(!is.na(crop_r), sum)

# create one cropland-presence layer:
#    1  = at least one crop is present
#    0  = valid data, but no crops are present
#    NA = all crop layers are missing
cropP_r = app(crop_r > 0, max, na.rm = TRUE)
cropP_r = ifel(n_valid == 0, NA, cropP_r)

# update name
names(cropP_r) = 'cropland_binary'

# combine layers
r = c(r1,cropP_r)
#-------------------------------------------------------------------------------
# SAVE RASTER
#-------------------------------------------------------------------------------
writeRaster(r, paste0(o_data, '/abovegroundNPP-percent-removed-',args[1],'.tif'), overwrite = TRUE)
