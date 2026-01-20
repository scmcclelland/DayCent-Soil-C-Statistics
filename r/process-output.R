# DayCent Simulation Model Output Results Processing for Analysis #

# file name:    process-output.R
# author:       S.C. McClelland
# created:      24 July 2023
# last updated: 20 January 2026

# description: This file contains a script to automate raw result processing of global
#              DayCent simulations on a local machine.
#              It does the following:
#                 - Unzips tar files
#                 - Creates separate dt for SOC subset by simulation years
#-----------------------------------------------------------------------------------------
# LIBRARIES
#-----------------------------------------------------------------------------------------
library(data.table)
library(sf)
library(stringr)
library(terra)
options(scipen = 999, digits = 4)
#-----------------------------------------------------------------------------------------
args = commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  stop(
    "Need 3 command-line arguments:\n",
    "  1) DayCent run selection for correct file selection\n",
    "  2) Final simulation year (integer)\n",
    "  3) scenario\n",
    "Example: Rscript r/process-output.R dec-23 2036 conv"
  )
}
# args[1] run selection based on DayCent runs
 if(args[1] == 'oct-23') {
  run_rows  = '1-480448'
} else if(args[1] == 'dec-23') {
  run_rows = '1-145676'
} else if (args[1] == 'feb-24') {
  run_rows = '1-778724' 
} else {
  print('This is not a valid argument for processing.')
  stop()
}
# args[2] simulation time
s_yr = suppressWarnings(as.integer(args[2]))
if (is.na(s_yr)) {
  stop("Final simulation year must be an integer, got: ", args[2])
}
yr_max = s_yr + 1L
# args[3] scenario
scenarios = args[3]
#-----------------------------------------------------------------------------------------
# FUNCTIONS
source('r/process-output-functions.R')
#-----------------------------------------------------------------------------------------
# DIRECTORIES
main.dir  = getwd() 
print(main.dir)
working.dir = file.path(main.dir, "test-data", "model-output", args[1]) # UPDATE
rda.dir = file.path(main.dir, "test-data", "uncertainty-input") # UPDATE
if (!dir.exists(working.dir)) {
  stop("Missing working directory: ", working.dir)
}
if (!dir.exists(rda.dir)) {
  dir.create(rda.dir, recursive = TRUE)
}
setwd(working.dir)
#-----------------------------------------------------------------------------------------
dirs = list.files(working.dir, pattern = "\\.tar\\.gz$", full.names = TRUE, recursive = FALSE)
if (length(dirs) == 0) {
  stop("No .tar.gz input files found in: ", working.dir)
}
print(dirs)

# STEP 1.1 Unzip tar files (as a loop)
if (args[1] %in% c('oct-23','dec-23', 'feb-24')) {
  for (dir in dirs)  {
    # unzip files
    print(paste0('unzipping tar.zip for ', dir))
    untar(dir)
    # resolve top-level extracted directory from tar contents
    tar_contents = untar(dir, list = TRUE)
    top_dir = unique(vapply(strsplit(tar_contents, "/"), `[`, character(1), 1))
    if (length(top_dir) != 1) {
      stop("Expected a single top-level directory in tar: ", dir)
    }
    dir_n = file.path(dirname(dir), top_dir)
    # file 1
    print(paste0('reading csv into R for ', dir_n))

name_f = list.files(file.path(dir_n, run_rows), 'daycent_simulation_output_rows_',
                   full.names = TRUE)
if (length(name_f) == 0) {
  stop("No matching output files found in: ", file.path(dir_n, run_rows))
}
sb_cmd = sprintf("awk -F',' 'NR==1 || $8 < %d' %s",
                 yr_max, name_f)
file   = fread(cmd = sb_cmd)
gc()
file   = unique(file)
gc()

# STEP 1.2 Process raw results
# STEP 1.2.1 SOC
print('Running SOC function.')
dt_SOC = process_SOC(file, scenarios, s_yr)

# SAVE
fname_SOC     = paste(dt_SOC[1, ssp], scenarios,'SOC-results.csv', sep = '-')
print(paste0('Saving SOC raw results file for ', dir))
fwrite(dt_SOC, file = paste(rda.dir, fname_SOC, sep = '/'), append = TRUE)

# STEP 1.3 REMOVE unzipped directory
    unlink(dir_n, recursive = TRUE)
    rm(file, dt_SOC)
    gc()
    print('Moving to next tar.gz')
  }
} else {
  print('This is not a valid option.')
}
